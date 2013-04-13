{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Top where

import Import

getTopR :: Handler RepHtml
getTopR = do
  ms <- runDB $ selectList [] []
  defaultLayout $ do
    setTitleI MsgWelcome
    $(widgetFile "top")
