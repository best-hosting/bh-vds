{-# LANGUAGE RankNTypes             #-}

module Sgf.Text.XML.Light.Proc
    ( qn
    , elQN
    --, elN
    , elN2
    --, attrQN
    , attrQN2
    --, attrN
    , attrN2
    --, elAttrQN
    , elAttrQN2
    , queryXMLPath
    , queryXMLPath'

    , onlyText'
    , onlyText2
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

elN :: Monoid r => String -> Element -> (r, Bool)
elN n x
  | qName (elName x) == n   = (mempty, False)
  | otherwise               = (mempty, True)

elN2 :: String -> Element -> Bool
elN2 n              = (n ==) . qName . elName

attrQN :: Monoid r => QName -> Attr -> (r, Bool)
attrQN qn x
  | attrKey x == qn = (mempty, False)
  | otherwise       = (mempty, True)

attrQN2 :: QName -> Attr -> Bool
attrQN2 qn          = (qn ==) . attrKey

attrN :: Monoid r => String -> Attr -> (r, Bool)
attrN n x
  | qName (attrKey x) == n  = (mempty, False)
  | otherwise               = (mempty, True)

attrN2 :: String -> Attr -> Bool
attrN2 n            = (n ==) . qName . attrKey

elAttrQN :: Monoid r => QName -> String -> Element -> (r, Bool)
elAttrQN qn v x
  | maybe False (== v) (findAttr qn x)  = (mempty, False)
  | otherwise                           = (mempty, True)

elAttrQN2 :: QName -> String -> Element -> Bool
elAttrQN2 qn v x
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


onlyText' :: [Content] -> String
onlyText'           = concatMap cdData . onlyText

onlyText2 :: [Content] -> T.Text
onlyText2           = T.pack . concatMap cdData . onlyText

