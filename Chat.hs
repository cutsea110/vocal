module Chat where

import Prelude
import Yesod
import Control.Concurrent.Chan (Chan, dupChan, writeChan)
import Data.Text (Text)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Language.Haskell.TH.Syntax (Type(VarT), Pred(ClassP), mkName)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Monoid (mappend)
import Text.Julius (rawJS)

data Chat = Chat (Chan ServerEvent)

class (Yesod master, RenderMessage master FormMessage) =>
       YesodChat master where
  getUserName :: GHandler sub master Text
  isLoggedIn :: GHandler sub master Bool

mkYesodSub "Chat"
  [ ClassP ''YesodChat [VarT $ mkName "master"]
  ] [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

postSendR :: YesodChat master => GHandler Chat master ()
postSendR = do
  from <- getUserName
  body <- runInputGet $ ireq textField "message"
  Chat chan <- getYesodSub
  liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
    fromText from `mappend` fromText " : " `mappend` fromText body

getReceiveR :: GHandler Chat master RepHtml
getReceiveR = do
  Chat chan0 <- getYesodSub
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- lift $ eventSourceAppChan chan req
  sendWaiResponse res

chatWidget :: YesodChat  master =>
  (Route Chat -> Route master) -> GWidget sub master ()
chatWidget toMaster = do
  chat <- lift newIdent
  output <- lift newIdent
  input <- lift newIdent
  ili <- lift isLoggedIn
  if ili
    then do
        [whamlet|
<div ##{chat}>
  <h2>Chat
  <div ##{output}>
  <input ##{input} type=text placeholder="Enter Message">
|]
        toWidget [lucius|
 ##{chat} {
  top: 2em;
 }
 ##{output} {
  width: 80%;
  height: 300px;
  border: 1px solid #999;
  overflow: auto;
 }
|]
        toWidgetBody [julius|
var output = document.getElementById("#{rawJS output}");
var src = new EventSource("@{toMaster ReceiveR}");
src.onmessage = function(msg){
  var p = document.createElement("p");
  p.appendChild(document.createTextNode(msg.data));
  output.appendChild(p);
  output.scrollTop = output.scrollHeight;
};
var input = document.getElementById("#{rawJS input}");
input.onkeyup = function(event){
  var keycode = (event.keyCode ? event.keyCode : event.which);
  if (keycode == '13') {
    var xhr = new XMLHttpRequest();
    var val = input.value;
    input.value = "";
    var params = "?message=" + encodeURI(val);
    xhr.open("POST", "@{toMaster SendR}" + params);
    xhr.send(null);
  }
};
|]
    else do
        master <- lift getYesod
        [whamlet|
<p>
  You must be #
  $maybe ar <- authRoute master
    <a href=@{ar}>logged in
  $nothing
    logged in to chat.
|]
