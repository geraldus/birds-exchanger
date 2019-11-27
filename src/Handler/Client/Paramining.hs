module Handler.Client.Paramining where

import           Import

import           Data.Maybe                ( listToMaybe )
import           Local.Params              ( defaultParaMiningDelaySeconds )
import           Type.Wallet               ( WalletData (..) )
import           Utils.Database.Operations ( accrueParaminingDB )

import           Control.Concurrent        ( forkIO, threadDelay )
import qualified Data.Map                  as M
import           Text.Pretty.Simple        ( pShow )


accrueParamining ::
        WalletData -> Handler (Maybe (Entity WalletBalanceTransaction))
accrueParamining stats =
        listToMaybe <$> runDB (accrueParaminingDB stats)

scheduleParaminingAccrual :: WalletData -> Handler ()
scheduleParaminingAccrual stats = do
    runHandler <- handlerToIO
    liftIO . forkIO $ do
        threadDelay $
            defaultParaMiningDelaySeconds * 1000 * 1000
        runHandler (accrueParamining stats >> return ())
    return ()

scheduleParaminingMapAccrual ::
        M.Map UserWalletId WalletData -> Handler ()
scheduleParaminingMapAccrual paraMap = do
    runHandler <- handlerToIO
    liftIO $ do
        forkIO $ do
            putStrLn . toStrict . pShow $ paraMap
            threadDelay $
                defaultParaMiningDelaySeconds * 1000 * 1000
            runHandler . runDB $ mapM_
                ((const (pure ()) =<<) . accrueParaminingDB . snd)
                (M.toList paraMap)
        return ()

