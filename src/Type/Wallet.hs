{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Type.Wallet where

import           Import.NoFoundation
import           Local.Persist.Currency ( currencyCodeT' )
import           Type.Money             ( Percent, percentToJSON )

import           Data.Time.Clock.POSIX  ( utcTimeToPOSIXSeconds )


-- | Represents wallet balance statistics
data WalletData = WalletData
    { walletDataWallet          :: Entity UserWallet
    , walletDataOrdersCents     :: Int
    -- ^ actual amount of cents held within active orders, in other words
    -- how many available cents left in orders
    , walletDataWithdrawalCents :: Int
    -- ^ actual amount of cents help within yet unexecuted withdrawals requests,
    -- in other words how many cents could be returned to balance if all
    -- of withdrawal requests will be cencelled
    , walletDataLastParaTime    :: Maybe UTCTime
    -- ^ last operation lead to paramining time or paramining accrual time
    -- strictly queried from database
    , walletDataParaminingRate  :: Maybe Percent
    -- ^ monthly paramining rate for current wallet
    }
    deriving Show

-- | JSON Representation
instance ToJSON WalletData where
    toJSON WalletData{..} = object
        [ "wallet" .= walletJSON
        , "orders" .= toJSON walletDataOrdersCents
        , "withdrawal" .= toJSON walletDataWithdrawalCents
        , "lastParaTime" .= utcTimestampJSON lastParaTime
        , "monthlyParaminingRate" .= percentToJSON' walletDataParaminingRate ]
      where
        percentToJSON' x = case x of
            Nothing -> Null
            Just p -> percentToJSON p
        walletId = entityKey walletDataWallet

        wallet = entityVal walletDataWallet

        walletJSON = object
            [ "id" .= toJSON walletId
            , "token" .= userWalletWalletId wallet
            , "currency" .= toJSON
                    (toLower . currencyCodeT' . userWalletCurrency $ wallet)
            , "balance" .= toJSON (userWalletAmountCents wallet)
            , "lastParaTransactionTime" .= toJSON (
                        toUnixTimestampDouble
                        <$> userWalletLastParaTime wallet )
            ]

        lastParaTime = walletDataLastParaTime

        utcTimestampJSON = toJSON . (toUnixTimestampDouble <$>)

        toUnixTimestampDouble = toDouble . utcTimeToPOSIXSeconds

        toDouble :: Real a => a -> Double
        toDouble = realToFrac
