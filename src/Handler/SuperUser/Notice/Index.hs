module Handler.SuperUser.Notice.Index where

import           Import

import           Handler.SuperUser.Notice.Paramining.UserList ( defaultLimit )


getSUNoticeIndexR :: Handler Html
getSUNoticeIndexR = defaultLayout
    $(widgetFile "su/notice/index")
