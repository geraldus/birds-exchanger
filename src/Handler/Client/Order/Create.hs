{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Client.Order.Create where

import           Import

import           Form.Exchanger.Order
import           Local.Params              ( defaultExchangeFee )
import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Utils.Database.Operations
import           Utils.Money


data ProcessForm
    = ProcessFormNoData
    | ProcessFormErrors [ LabeledError ]
    | ProcessFormSuccess
            AmountCents Currency UTCTime (UserWallet -> OrderCheck)


processNewOrder
    :: UserId
    -> ExchangePair
    -> AmountCents
    -> NormalizedRatio
    -> UTCTime
    -> (AppMessage -> Text)
    -> ProcessForm
processNewOrder client epair a r t mr =
    let er a' r' w' = orderCreateRenderFormErrors a' r' w' mr
        chk w = checkOrderData epair a r client w t er
        c = fst . unPairCurrency $ epair
    in ProcessFormSuccess a c t chk

checkOrderData
    :: ExchangePair
    -> AmountCents
    -> NormalizedRatio
    -> UserId
    -> UserWallet
    -> UTCTime
    -> (Int -> Double -> UserWallet -> [ LabeledError ])
    -> OrderCheck
checkOrderData
        exchangeDirection
        amount
        ratio
        client
        wallet
        time
        renderErrors =
    if amount <= 0 || ratio <= 0 || amount > userWalletAmountCents wallet
        then OrderCheckErrors (renderErrors amount ratio wallet)
        else
            let r = pairRatioByNormalizedRatio exchangeDirection ratio
                -- Ratio Multiplicator
                -- Ratio within form data is in normalized state, convert it
                -- to direct multiplication value
                t = multiplyCents r amount
                -- Target Amount
                -- Amount cents required for full order exchange
                f = calcFeeCents defaultExchangeFee t
                -- Expected fee, shown for user when order was created
                -- d = giveTake action pair (flipPair epair)
                d = exchangeDirection
                -- Correct Exchange Direction
                -- Pair within form data doesn't represent correct exchange
                -- direction, it must be interpreted depending on 'action', e.g.
                -- give action -- same direction, take action -- flipped.
            in OrderCheckSuccess $
                    mkNewOrderData client amount ratio d f time

orderCreateRenderFormErrors
    :: AmountCents
    -> NormalizedRatio
    -> UserWallet
    -> (AppMessage -> Text)
    -> [ LabeledError ]
orderCreateRenderFormErrors a r w m =
    let amount = mkError (a <= 0) "amount" (m MsgPositiveValueRequired)
        ratio  = mkError (r <= 0) "ratio" (m MsgPositiveValueRequired)
        balance = mkError
                (userWalletAmountCents w < a) "balance" (m MsgNotEnoughFunds)
    in amount <> ratio <> balance
  where mkError cond name e = [ (name, e) | cond ]


giveTake :: ExchangeAction -> p -> p -> p
giveTake a giveChoise takeChoise = if a == EAGive
    then giveChoise else takeChoise


postExchangeOrderCreateR' :: ExchangePair -> Handler Html
postExchangeOrderCreateR' epair = do
    client <- requireClientId
    rid <- newIdent
    wid <- newIdent
    ((res, _), _) <- runFormPost $ createOrderForm wid rid epair
    proceessResult <- case res of
        FormMissing    -> return ProcessFormNoData
        FormFailure es -> return $
                ProcessFormErrors $ zip es (repeat "form")
        FormSuccess od -> do
            let act = action od
                r = ratio od
                formPair = pair od
                d = giveTake act formPair (flipPair formPair)
            let a = case act of
                        EAGive -> amount od
                        EAReceive ->
                            let k = pairRatioByNormalizedRatio formPair r
                            in multiplyCents k (amount od)
            mr <- getMessageRender
            t <- liftIO getCurrentTime
            return $ processNewOrder client d a r t mr
    case proceessResult of
        ProcessFormErrors es -> do
            -- TODO: FIXME: Add JSON response capabilities
            let msg = [shamlet|$forall (_, message) <- es
                    <div>#{message} |]
            setMessage msg
        ProcessFormSuccess a c t checkOrder -> do
            runDB $ saveAndExecuteOrder client a c t checkOrder
            return ()
        _ -> return ()
    redirect HomeR


postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = postExchangeOrderCreateR' ExchangePzmRur



{-

Пользователь
Отдаёт валюту А, X штук
Хочет валюту Б, Y штук
K1  = X/Y
K1' = Y/X

Существует
Отдаю валюту Б, W штук
Хочу валюту А, Z шук
K2  = Z/W
K2' = W/Z

Сделки не будет, если
K1  <  K2
K1' >  K2'

Поиск подходящего ордера: смотрим какое направление соотношения, это
определяет K или K' используем для сравнения.
k1  >= k2 или k1' <= k2'

При равных k1 == k2 (=> k1' == k2') можем выиграть на преобразовании

Рассмотрим сначала случай с равными соотношениями K1 == K2 и K1' == K2'
Если X == Z (тогда обязательно будет Y == W), то мы просто получаем 1%
с каждой стороны.

Если X > Z (что означает, что Y > W) то мы можем немного выиграть на
округлении. Если пользователь отдаёт больше, чем существующий хочет
получить, значит существующий ордер будет полностью закрыт, эти значения
должны быть точными, а сыграть на округлении мы сможем только при
переводе пользователю.  Переводиться будет точная сумма, а вот
начисляться будет путём преобразования:
y' = x' * k = x' / k' = z * k = z / k'
Получить разницу мы сможем, если y' < w

Аналогично, обратное
Z > X ==> W > Y
Пользователь отдаёт меньше, чем существущий хочет принять.
В этом случае пользовательский ордер будет закрыт полностью и поэтому
так нужно указать точные цифры, а существующий будет исполнен частично.
Отадавать сущ-ий будет точную цифру, то есть w' = x, а вот принимать
по формуле
z' = w' * k = w' / k' = x * k = x / k'
Разница наша, если z' < x


Теперь рассмотрим, что будет при
k1 > k2 (=> k1' < k2')

При обмене один ордер всегда будет ичсерпан (закрыт).

Возможны три ситуации:
x * k1' > w
x * k1' = w
x * k1' < w

1. Пользователь отдать готов больше, чем существуюший хочет получить
x > z


Мы сможем заработать, когда:
1. Пользователь отдаёт А дороже, чем существующий хочет за неё.
Заработаем на пользователе. Иными словами, за 1 А пользователь даёт
больше Б, чем существуюший хочет получить.  Иными словами, за 1 А
пользователь расчитывает получить меньше Б, чем существующий ордер
готов отдать. Получается, что можем заработать и на существующем ордере.

k1' > k2'
k2  > k1

Как же понять, на ком мы заработаем?  Так как при обмене всегда будет
закрывать один из ордеров, то:
1. закрывающийся ордер нужно выполнить точно, начислить ему
по установленному в оредере курсу монеты и списать существующую сумму.
А второму начислить ровно столько, сколько он ждёт за
переведённые деньги.  Как выяснить, какой ордер будет закрыт?

Пользователь отдёт Х А, существующий ждёт Z А.  Если
| x > z
| x > w * k2 > w / k2' -- заработаем на пользователе.


| y = x * k1 = x / k1'
| w = z * k2' = z / k2
| k1 < k2; k1' > k2'

y ?? w
x * k1 ?? w
x * k1 ?? z / k2
x * k1 * k2 ?? z
x > z, а это значит, что y > w,
если k1 * k2 >= 1.  Если k1 * k2 < 1, тогда оценить однозначно
нет возможности.

x < z
Пользователь может отдать А меньше, чем Существующий хочет получить.

В этих случаях нужно выяснить, кто хочет меньше. x < z || x > z
Отдаём всё тому, кто хочет меньше.  У него снимаем все средства
и закрываем ордер.

Второму переводим столько сколько он хотел.  Как есть.  Именно эту сумму
списываем у второго, но начисляем по расчёту на основе его соотношения.

x > z
То есть второй хочет получить меньше, чем даёт первый.
Закрываем второго.
Переводим ему Z - 1%
Его сумму W мы уже держим на руках.
Забираем у первого (списываем с ордера) x' = z
Разсчитываем сколько перевести:
y' = x' * k1' (= x' / k1)
По идее мы выигрываем на y, то есть на валюте Б.  Сравним y' и w

y' = x' * k1' = z * k1'
w = z * k2 = z / k2'

y' ?? w
z * k1' ?? z / k2'
k1' * k2' ??  1

****
k1 * k2' = 1, k1 == k2
k1 * (1/k2) = 1, k1 == k2
k1 < k2
k1 * (1/k2)


Как определить кто хочет больше?  Просто сравнить x и z.



2.


x = y * k1'
x = y / k
z = w * k2
x = w / k2'



k1' < k2'



Ход мыслей

Пользователь надеется получить Б
за одну Б отдаёт K1' А
за одну А хочет  K1  Б

Проверкка:
x = 10  (a = pzm)
y = 100 (b = rur)
k1  = 0.1 (1/10)  k pzm -> 1 rur
k1' = 10          k rur -> 1 pzm
-----
Верно
-----

Сделки не будет, если существующий ордер готов за одну А
предлагать меньше Б, чем расчитывает пользователь.  В виде формулы:
k2' < k1'
Так же сделки не будет, если существуюший ордер за 1 Б хочет больше,
чем пользователь готов пледложить, то есть
k2  > k1



-}

