{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.API.Stocks where

import           Import
import           Local.Persist.UserRole ( UserRole (Admin, Operator, SuperUser) )

import           Database.Persist.Sql   ( toSqlKey )


postAPIStocksOperatorCancelPurchaseR :: Handler TypedContent
postAPIStocksOperatorCancelPurchaseR = do
    withStaffUserAuthenticated $ do
        form <- runOperatorStocksPurchaseCancelForm
        case form of
            FormMissing     -> responseJsonMissingFormData
            FormFailure _   -> responseJsonInvalidArgs
            FormSuccess (pid, note) -> do
                res <- genericCancelPurchase pid note doNotCancelAccepted
                case res of
                    AuthorizationRequired -> responseJsonAuthenticationRequired
                    NotAuthorized         -> responseJsonNotAuthorized
                    NoSuchObject          -> responseJsonObjectNotFound
                    Success purchase      -> selectRep $ do
                        provideRep . pure $
                            object [ "status" .= ("ok" :: Text)
                                   , "data" .= toJSON purchase ]
                        provideRep $
                            (redirectUltDest OperatorStocksPurchaseIndexR :: Handler Html)
  where
    doNotCancelAccepted = False

    runOperatorStocksPurchaseCancelForm = runInputPostResult $
        (,)
            <$> (toSqlKey <$> ireq intField "stocks-purchase-id")
            <*> ((unTextarea <$>) <$> iopt textareaField "note")


withStaffUserAuthenticated :: Handler TypedContent -> Handler TypedContent
withStaffUserAuthenticated action = do
    usr <- maybeAuthPair
    case usr of
        Nothing -> responseJsonAuthenticationRequired
        Just (Right _, Right _) -> action
        Just (Left _, Left (User _ _ Admin)) -> action
        Just (Left _, Left (User _ _ Operator)) -> action
        _ -> responseJsonNotAuthorized

genericCancelPurchase :: StocksPurchaseId
                      -> Maybe Text
                      -> Bool
                      -> Handler ApiStocksPurchaseCancelResponse
genericCancelPurchase pid note cancelAccepted = do
    client <- maybeAuthPair
    case client of
        Nothing -> return AuthorizationRequired
        Just user -> do
            pdb <- queryStocksPurchase
            case pdb of
                Nothing -> return NoSuchObject
                Just p -> do
                    let isAccepted = isJust (stocksPurchaseAccepted p)
                    case user of
                        (Right _, Right _) ->
                            if isAccepted
                                then
                                    if cancelAccepted
                                    then cancelPurchase SuperUser
                                    else return NotAuthorized
                                else cancelPurchase SuperUser
                        (Left uid, Left (User _ _ role)) -> do
                            let isStaff   = role `elem` [Admin, Operator]
                                isOwner   = uid == stocksPurchaseUser p
                            if not (isStaff || isOwner)
                            then return NotAuthorized
                            else
                                if isAccepted
                                then
                                    if cancelAccepted
                                    then cancelPurchase role
                                    else return NotAuthorized
                                else cancelPurchase role
                        _ -> return NotAuthorized
  where
    queryStocksPurchase = runDB $ get pid

    cancelPurchase role = do
        let msg = case role of
                SuperUser -> MsgCancelledBySuperUser
                Admin     -> MsgCancelledByAdmin
                Operator  -> MsgCancelledByOperator
                _         -> MsgCancelledByUser
        render <- getMessageRender
        now    <- liftIO $ getCurrentTime
        let note' = fromMaybe (render msg) note
        Success . Entity pid <$> markPurchaseCancelled now note'

    markPurchaseCancelled t n = runDB $
        updateGet pid [ StocksPurchaseCancelled =. Just t
                      , StocksPurchaseCancellationNote =. Just n ]


type ApiStocksPurchaseCancelResponse
    = ApiRecordManipulationResponse StocksPurchase

data ApiRecordManipulationResponse a
    = AuthorizationRequired
    | NotAuthorized
    | NoSuchObject
    | Success (Entity a)


responseJsonAuthenticationRequired :: Handler TypedContent
responseJsonAuthenticationRequired = do
    render <- getMessageRender
    let msg = MsgAPIAuthorizationRequired
    selectRep . provideRep . pure . object $
        [ "status" .= ("fail" :: Text)
        , "message" .= render msg ]

responseJsonNotAuthorized :: Handler TypedContent
responseJsonNotAuthorized = do
    render <- getMessageRender
    let msg = MsgAPINotAuthorized
    selectRep . provideRep . pure . object $
        [ "status" .= ("fail" :: Text)
        , "message" .= render msg ]

responseJsonObjectNotFound :: Handler TypedContent
responseJsonObjectNotFound = do
    render <- getMessageRender
    let msg = MsgAPITargetOjectNotFound
    selectRep . provideRep . pure . object $
        [ "status" .= ("fail" :: Text)
        , "message" .= render msg ]

responseJsonMissingFormData :: Handler TypedContent
responseJsonMissingFormData = do
    render <- getMessageRender
    let msg = MsgAPIMissingFormData
    selectRep . provideRep . pure . object $
        [ "status" .= ("fail" :: Text)
        , "message" .= render msg ]

responseJsonInvalidArgs :: Handler TypedContent
responseJsonInvalidArgs = do
    render <- getMessageRender
    let msg = MsgAPIInvalidFormData
    selectRep . provideRep . pure . object $
        [ "status" .= ("fail" :: Text)
        , "message" .= render msg ]
