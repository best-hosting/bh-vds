{-# LANGUAGE RankNTypes             #-}

-- |
-- Module: Sgf.Text.XML.Light.Proc
--
-- Predicates and operations on xml values created by "Text.XML.Light".

module Sgf.Text.XML.Light.Proc
    ( qn
    , elN
    , attrN
    , elAttrVal
    , queryXMLPath
    , queryXMLPath'

    , onlyText'
    , onlyTextT
    )
  where

import           Data.Generics
import qualified Data.Text              as T
import           Control.Applicative
import           Control.Monad.Reader
import           Text.XML.Light

import           Sgf.Data.Generics.Schemes


-- | Make 'QName' from 'String'.
qn :: String -> QName
qn s            = QName {qName = s, qURI = Nothing, qPrefix = Nothing}

-- | Predicate matching 'qName' of 'Element'.
elN :: String -> Element -> Bool
elN n               = (n ==) . qName . elName

-- | Predicate matching 'qName' of 'Attr'.
attrN :: String -> Attr -> Bool
attrN n             = (n ==) . qName . attrKey

-- | Predicate matching value of 'Attr' with specified 'QName' in an 'Element'
elAttrVal :: QName -> String -> Element -> Bool
elAttrVal aqn v x
  | maybe False (== v) (findAttr aqn x) = True
  | otherwise                           = False


-- | Function for modifying @Reader [QName]@ context by adding 'QName' of
-- current 'Element'.
setQName :: GenericQ ([QName] -> [QName])
setQName            = id `mkQ` ((:) . elName)

-- | Query each element in context of its path.
queryXMLPath :: (r -> r -> r) -> GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath k q    = flip runReader []
                        . everythingR setQName (liftA2 k) (reader <$> q)

-- | Monoid version of 'queryXMLPath'.
queryXMLPath' :: Monoid r => GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath'       = queryXMLPath mappend


-- | 'onlyText' version concatenating all text from a list
-- of XML contents.
onlyText' :: [Content] -> String
onlyText'           = concatMap cdData . onlyText

-- | 'onlyText'' version returning 'T.Text'.
onlyTextT :: [Content] -> T.Text
onlyTextT           = T.pack . onlyText'

