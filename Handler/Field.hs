{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Field where

import Import

getFieldR :: Text -> Handler RepHtml
getFieldR group = do
  m <- runDB $ getBy404 $ UniqueMinority group
  defaultLayout $ do
    setTitleI $ MsgField group
    $(widgetFile "field")
