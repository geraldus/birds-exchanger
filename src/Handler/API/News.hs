module Handler.API.News where

import           Import


getAPINewsListR :: Handler TypedContent
getAPINewsListR = do
    list <- runDB $ selectList [] [Desc InfoCreated]
    selectRep . provideRep . pure $ toJSON list
--   where
--     newsJSON = jsonMerge

