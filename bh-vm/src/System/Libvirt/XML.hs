{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes         #-}

module System.Libvirt.XML
    ( volumeXml
    , readVolumeXml

    , domainXml
    , readDomainXml
    , initDomain
    )
  where

import Data.Monoid
import Data.Maybe
import Data.Generics
import qualified Data.Text as T
import Control.Monad.IO.Class
import Text.XML.Light
import qualified Text.XML.Light.Lexer as X
import qualified Filesystem.Path.CurrentOS as F

import Sgf.Common
import Sgf.Control.Lens
import System.Libvirt.Types
import Sgf.Data.Generics.Aliases
import Sgf.Data.Generics.Schemes
import Sgf.Text.XML.Light.Proc


volDiskXml :: GenericRecQ (Endo Volume, Bool)
volDiskXml          = endParserQL volPathL
                            (pure . toFirst . filePath . onlyText')
                        `extRecL` pureQ (elN "path")

-- | Generic query for 'Volume' working on a libvirt storage @volume@ xml.
volumeXml :: GenericRecQ (Endo Volume, Bool)
volumeXml           = mkRecL vol `extRecL` pureQ (elN "volume")
  where
    vol :: Element -> RecB (Endo Volume)
    vol x
      | elN "capacity"  x   = next $ volSizeL `endParserQL` (fmap toLast . parseSize . onlyTextT)
      | elN "name"      x   = next $ volNameL `endParserQL` (parseName . onlyTextT)
      | elN "target"    x   = next $ volDiskXml
      | otherwise           = stop mempty

-- | Parse 'Volume' from an 'XmlSource' containing libvirt @volume@ xml.
readVolumeXml :: X.XmlSource s => s -> Volume
readVolumeXml       = ($ mempty) . appEndo
                        . everythingRecBut mappend volumeXml
                        . parseXML


-- | Generic query for '_arch' working on a /partial/ xml tree.
archXml :: GenericRecQ (Endo Domain, Bool)
archXml             = endParserQL archL (parseArch . T.pack)
                        `extRecL` pureQ (attrN "arch")
                        `extRecL` pureQ (elN "type")

-- | Generic query for @file@ attribute in @source@ element.
sourceFileXml :: Monoid a => (String -> a) -> GenericRecQ (a, Bool)
sourceFileXml p     = endQ p
                        `extRecL` pureQ (attrN "file")
                        `extRecL` pureQ (elN "source")

-- | Generic query for @dev@ attribute in @source@ element.
sourceDevXml :: Monoid a => (String -> a) -> GenericRecQ (a, Bool)
sourceDevXml p      = endQ p
                        `extRecL` pureQ (attrN "dev")
                        `extRecL` pureQ (elN "source")

-- | Generic query for 'Interface' inside 'Domain' working on a /partial/ xml
-- tree.
bridgeXml :: GenericRecQ (Endo Domain, Bool)
bridgeXml           = endParserQL bridgeL (fmap toLast . parseIntName . T.pack)
                        `extRecL` pureQ (attrN "bridge")
  where
    parseIntName :: T.Text -> Either String Interface
    parseIntName x  = Interface <$> parseName x

-- | Generic query for 'IP' inside 'Domain' working on a /partial/ xml tree.
domIpXml :: GenericRecQ (Endo Domain, Bool)
domIpXml            = endParserQL ipL (fmap toLast . parseIP . T.pack)
    `extRecL` pureQ (attrN "value")
    `extRecL` pureQ (elN "parameter" <&&> elAttrQN (qn "name") "IP")

-- | Generic query for values in 'Domain' initialized from the @interface@
-- block in libvirt domain xml tree (works on /partial/ xml tree).
domInterfaces :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domInterfaces x
  | elN "source"    x   = next bridgeXml
  | elN "filterref" x   = next domIpXml
  | otherwise           = stop mempty

-- | Generic query for values in 'Domain' initialized from the @devices@ block
-- in libvirt domain xml tree (works on /partial/ xml tree). Note, that each
-- found '_volPath' is decoded as /separate/ 'Volume'.
domDevices :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domDevices x
  | elN "interface" x   = next (mkRecL domInterfaces)
  | elN "disk" x && elAttrQN (qn "device") "cdrom" x = next $
                          sourceFileXml (Endo . setA cdromL . toFirst . filePath)
  | elN "disk" x && elAttrQN (qn "device") "disk" x  = next $
                          sourceDevXml (\y -> Endo $ setA volumeL (volDisk y))
  | otherwise           = stop mempty
  where
    volDisk :: String -> Volume
    volDisk t       = setA volPathL (toFirst (filePath t)) mempty

-- | Generic query for 'Domain' working on a libvirt @domain@ xml.
domainXml :: GenericRecQ (Endo Domain, Bool)
domainXml           = mkRecL dom `extRecL` pureQ (elN "domain")
  where
    dom :: Element -> RecB (Endo Domain)
    dom x
      | elN "name"    x = next $ nameL `endParserQL` (parseName . onlyTextT)
      | elN "os"      x = next archXml
      | elN "memory"  x = next $ memoryL `endParserQL` (fmap toLast . parseSize . onlyTextT)
      | elN "vcpu"    x = next $ vcpuL   `endParserQL` (fmap toLast . parseVCpu . onlyTextT)
      | elN "devices" x = next (mkRecL domDevices)
      | otherwise       = stop mempty

-- | Parse 'Domain' from an 'XmlSource' containing libvirt @domain@ xml. Note,
-- that 'Volume'-s will be partially initialized, because libvirt @domain@ xml
-- does not contain all required information (only '_volPath' in fact).
readDomainXml :: X.XmlSource s => s -> Domain
readDomainXml       = ($ mempty) . appEndo
                        . everythingRecBut mappend domainXml
                        . parseXML

-- | Initialize 'Domain' from an libvirt @domain@ xml and query additional
-- information about each 'Volume' using supplied function and initialize it.
initDomain :: forall m s. (MonadIO m, X.XmlSource s) =>
              (F.FilePath -> m s) -> s -> m Domain
initDomain f        = modifyAA volumeL initVols . readDomainXml
  where
    -- | I need that type-signature, otherwise 'Traversable' constraint of
    -- 'mapM' can't be solved.
    initVols :: Volume -> m Volume
    initVols v
      | Just p  <- getFirst (volPath v) = do
            vx <- f (getPath p)
            let v'  = readVolumeXml vx
                vv' =  v <> v'
            if vv' /= v' <> v
              then error $ "Volume info does not match to domain.\n"
                    ++ "Read volume: " ++ show v' ++ "\n"
                    ++ "Domain's volume: " ++ show v
              else return vv'
      | otherwise   = return v

-- $utils

-- | Use a function (usually, result of parsing something with 'parseOnly') to
-- construct a final 'endQ' generic query, which assigns parsed value using
-- specified 'LensA'. Note, that if i'll take 'Parser b' here instead of 's ->
-- Either e b', i can't make 'endParserQL' to work on any input, like now.
-- Also with current type i may use 'endOfInput' in specific type parsers,
-- because they're evaluated till the end with 'parseOnly' in their specific
-- functions.
endParserQL :: Typeable s => LensA a b -> (s -> Either e b)
               -> GenericRecQ (Endo a, Bool)
endParserQL l p     = endQ $ either mempty (Endo . setA l) . p

-- | Construct a proper 'Monoid' from a 'FilePath'.
filePath :: String -> Path
filePath            = Path . F.decodeString

