{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.App.Common where

import           Import


{- YESOD.  APP -}

getRenders :: WidgetFor App (Route App -> Text, AppMessage -> Text)
getRenders = (,) <$> liftHandler getUrlRender <*> liftHandler getMessageRender

setCompositeTitle ::
       (MonadWidget m, HandlerFor site ~ m, RenderMessage site msg)
    => [ msg ] -> m ()
setCompositeTitle ms = do
    r <- getMessageRender
    setTitle . toHtml . intercalate " | " . map r $ ms
