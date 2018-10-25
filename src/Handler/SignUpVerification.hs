{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.SignUpVerification where


import           Import


getSignUpVerifyR :: Text -> Text -> Handler Html
getSignUpVerifyR email verkey = do
    result <- runDB $ do
        mayEmail <- getBy $ UniqueEmail email
        case mayEmail of
            Just (Entity emailId emailRec) ->
                if emailVerkey emailRec == Just verkey
                    then do
                        update emailId $ [EmailVerkey =. Nothing]
                        return Verified
                    else if isNothing (emailVerkey emailRec)
                        then return VerifiedAlready
                        else return InvalidKey
            Nothing -> return InvalidEmail
    defaultLayout $ case result of
        Verified -> do
            setTitle "Проверка почты успешно пройдена!"
            redirect HomeR
        VerifiedAlready -> do
            setTitle "Проверка уже пройдена"
            redirect HomeR
        _ -> $(widgetFile "auth/invalid-verification-data")


postSignUpVerifyR :: Text -> Text -> Handler Html
postSignUpVerifyR email verkey = return mempty


data VerificationResult
    = Verified
    | VerifiedAlready
    | InvalidKey
    | InvalidEmail
