module Helpers.Util
       ( newIdent2
       , newIdent3
       , newIdent4
       , toGravatarHash
       , gravatarUrl
       , fst3
       , snd3
       , thd3
       , fst4
       , snd4
       , thd4
       , frh4
       ) where

import Prelude
import Yesod
import Control.Applicative ((<$>),(<*>))
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Char (toLower, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.Text (Text)
import qualified Data.Text as T

newIdent2 :: Yesod m => GHandler s m (Text, Text)
newIdent2 = (,) <$> newIdent <*> newIdent

newIdent3 :: Yesod m => GHandler s m (Text, Text, Text)
newIdent3 = (,,) <$> newIdent <*> newIdent <*> newIdent

newIdent4 :: Yesod m => GHandler s m (Text, Text, Text, Text)
newIdent4 = (,,,) <$> newIdent <*> newIdent <*> newIdent <*> newIdent

toGravatarHash :: Text -> Text
toGravatarHash = T.pack . show . md5 . BL.fromString . map toLower . trim . T.unpack
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

gravatarUrl :: Int -> Text -> Text
gravatarUrl s h = T.concat [ "https://secure.gravatar.com/avatar/"
                           , h
                           , "?d=identicon&s="
                           , T.pack $ show s
                           ]

fst3 :: (a, b, c) -> a
fst3 (f,_,_) = f
snd3 :: (a, b, c) -> b
snd3 (_,s,_) = s
thd3 :: (a, b, c) -> c
thd3 (_,_,t) = t
fst4 :: (a, b, c, d) -> a
fst4 (f,_,_,_) = f
snd4 :: (a, b, c, d) -> b
snd4 (_,s,_,_) = s
thd4 :: (a, b, c, d) -> c
thd4 (_,_,t,_) = t
frh4 :: (a, b, c, d) -> d
frh4 (_,_,_,f) = f
