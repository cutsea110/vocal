{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Top where

import Import

getTopR :: Handler RepHtml
getTopR = do
  defaultLayout $ do
    setTitleI MsgWelcome
    $(widgetFile "top")
