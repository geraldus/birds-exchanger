
module Handler.Utils.Wallet where


import Import
import Local.Persist.Currency


getOrCreateWallet :: UserId -> Currency -> Handler (Entity UserWallet)
getOrCreateWallet userId currency = do
    walletTextId <- appNonce128urlT
    time <- liftIO getCurrentTime
    let newWallet = UserWallet userId currency 0 walletTextId time
    eitherWallet <- runDB $ insertBy newWallet
    return $ case eitherWallet of
        Left entity -> entity
        Right wid -> Entity wid newWallet