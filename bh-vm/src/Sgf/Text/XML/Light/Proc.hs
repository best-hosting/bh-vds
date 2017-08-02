{-# LANGUAGE RankNTypes             #-}

module Sgf.Text.XML.Light.Proc
    ( qn
    , elQN
    , elN
    , attrQN
    , attrN
    , elAttrQN
    , queryXMLPath
    , queryXMLPath'

    , onlyTextT
    , onlyText'
    )
  where

import Data.Generics
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Reader
import Text.XML.Light

import Sgf.Data.Generics.Schemes


-- | Make 'QName' with only 'qName' set.
qn :: String -> QName
qn s            = QName {qName = s, qURI = Nothing, qPrefix = Nothing}

elQN :: Monoid r => QName -> Element -> (r, Bool)
elQN qn x
  | elName x == qn  = (mempty, False)
  | otherwise       = (mempty, True)

elN :: String -> Element -> Bool
elN n               = (n ==) . qName . elName

attrQN :: QName -> Attr -> Bool
attrQN qn           = (qn ==) . attrKey

attrN :: String -> Attr -> Bool
attrN n             = (n ==) . qName . attrKey

elAttrQN :: QName -> String -> Element -> Bool
elAttrQN qn v x
  | maybe False (== v) (findAttr qn x)  = True
  | otherwise                           = False


-- | Function for modifying @Reader [QName]@ context by adding 'QName' of
-- current 'Element'.
setQName :: GenericQ ([QName] -> [QName])
setQName            = id `mkQ` ((:) . elName)

-- | Query each element in context of its path.
queryXMLPath :: (r -> r -> r) -> GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath k q    = flip runReader []
                        . everythingR setQName (liftA2 k) (reader <$> q)

queryXMLPath' :: Monoid r => GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath'       = queryXMLPath mappend


onlyTextT :: [Content] -> T.Text
onlyTextT           = T.pack . onlyText'

onlyText' :: [Content] -> String
onlyText'           = concatMap cdData . onlyText

