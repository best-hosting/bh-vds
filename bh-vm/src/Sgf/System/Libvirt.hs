{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sgf.System.Libvirt
    ( Name (..)
    , Size (..)
    , Pool (..)
    , Volume (..)
    , volumeXml
    , readVolumeXml

    , IP (..)
    , Interface (..)
    , Domain (..)
    , domainXml
    , readDomainXml
    , initDomain
    )
  where

import Data.Monoid
import Data.Maybe
import Data.Generics
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Text.XML.Light
import qualified Text.XML.Light.Lexer as X
import qualified Filesystem.Path.CurrentOS as F

import Sgf.Common
import Sgf.Data.Generics.Aliases
import Sgf.Data.Generics.Schemes
import Sgf.Data.Generics.Lenses
import Sgf.Text.XML.Light.Proc


-- | Type for names.
newtype Name        = Name (Last T.Text)
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'Name'.
parseName :: T.Text -> Either String Name
parseName           = Right . Name . Last . Just

-- | Type for sizes.
newtype Size        = Size (Sum Integer)
  deriving (Show, Typeable, Data, Eq, Ord, Num, Monoid)

-- | Parser for 'Size'.
parseSize :: T.Text -> Either String Size
parseSize           = parseOnly (Size . Sum <$> decimal)

-- | Type for libvirt storage pool.
newtype Pool        = Pool {_poolName :: Name}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Type for libvirt storage volume. Note, the type of '_volPath': 'FilePath'
-- 'Monoid' instance is wrong (it does not satisfy 'Monoid' laws), thus i need
-- a wrapper to fix it.
data Volume         = Volume
                        { _volName  :: Name         -- ^ Volume @name@.
                        , _volSize  :: Size         -- ^ Volume @capacity@.
                        , _volPath  :: Alt Maybe F.FilePath -- ^ Volume @target -> path@.
                        , _pool     :: Pool         -- ^ Volume pool.
                        }
  deriving (Show, Typeable, Data, Eq)

instance Monoid Volume where
    mempty          = Volume
                        { _volName  = mempty
                        , _volSize  = mempty
                        , _volPath  = mempty
                        , _pool     = mempty
                        }
    x `mappend` y   = Volume
                        { _volName  = _volName x <> _volName y
                        , _volSize  = _volSize x <> _volSize y
                        , _volPath  = _volPath x <> _volPath y
                        , _pool     = _pool x    <> _pool y
                        }

-- | Generic query for 'Volume' working on a libvirt storage @volume@ xml.
volumeXml :: GenericRecQ (Endo Volume, Bool)
volumeXml           = mkRecL vol `extRecL` pureQ (elN "volume")
  where
    vol :: Element -> RecB (Endo Volume)
    vol x
      | elN "capacity" x    = next $ endParserQ (parseSize . onlyTextT)
      | elN "name"     x    = next $ endParserQ (parseName . onlyTextT)
      | otherwise           = stop mempty

-- FIXME: Initialize '_volPath' in 'volumeXml'.

-- | Parse 'Volume' from an 'XmlSource' containing libvirt @volume@ xml.
readVolumeXml :: X.XmlSource s => s -> Volume
readVolumeXml       = ($ mempty) . appEndo
                        . everythingRecBut mappend volumeXml
                        . parseXML


-- | Type for architecture.
newtype Arch        = Arch (Last T.Text)
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'Arch'.
parseArch :: T.Text -> Either String Arch
parseArch           = parseOnly $ Arch . Last . Just
                        <$> (string "x86_64" <|> string "i686")

-- | Type for libvirt @vcpu@ definition inside domain.
newtype VCpu        = VCpu {_vcpu :: Sum Integer}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'VCpu'.
parseVCpu :: T.Text -> Either String VCpu
parseVCpu           = parseOnly (VCpu . Sum <$> decimal)

-- | Type for libvirt network @interface@ definition inside domain.
newtype Interface   = Interface {_intName :: Name}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'Interface'.
parseInterface :: T.Text -> Either String Interface
parseInterface      = (Interface <$>) . parseName

-- | Type for IP address.
data IP             = IP    { _octet1 :: Sum Int
                            , _octet2 :: Sum Int
                            , _octet3 :: Sum Int
                            , _octet4 :: Sum Int
                            }
  deriving (Show, Typeable, Data, Eq)

instance Monoid IP where
    mempty          = IP mempty mempty mempty mempty
    x `mappend` y   = IP    { _octet1 = _octet1 x `mappend` _octet1 y
                            , _octet2 = _octet2 x `mappend` _octet2 y
                            , _octet3 = _octet3 x `mappend` _octet3 y
                            , _octet4 = _octet4 x `mappend` _octet4 y
                            }

-- | Parser for 'IP'.
parseIP :: T.Text -> Either String IP
parseIP             = parseOnly $
                        IP <$> octet <*> octet <*> octet <*> octet
  where
    octet :: Parser (Sum Int)
    octet           = do
                        x <- decimal <* (string "." <|> endOfInput *> pure "")
                        case x of
                          _
                            | x > 255   -> error $
                                "Octet " ++ show x ++ " is too great."
                            | x < 0     -> error $
                                "Impossible happens: negative octet " ++ show x
                            | otherwise -> pure (Sum x)

-- | Type for libvirt @domain@. Note, the type of '_cdrom': 'FilePath'
-- 'Monoid' instance is wrong (it does not satisfy 'Monoid' laws), thus i need
-- a wrapper to fix it.
data Domain         = Domain
                        { _name     :: Name
                        , _arch     :: Arch
                        , _memory   :: Size
                        , _domVCpu  :: VCpu
                        , _cdrom    :: Alt Maybe F.FilePath
                        , _volume   :: [Volume]
                        , _bridge   :: Interface
                        , _domIp    :: IP
                        }
  deriving (Show, Typeable, Data, Eq)

instance Monoid Domain where
    mempty          = Domain
                        { _name     = mempty
                        , _arch     = mempty
                        , _memory   = mempty
                        , _domVCpu  = mempty
                        , _cdrom    = Alt Nothing
                        , _volume   = mempty
                        , _bridge   = mempty
                        , _domIp    = mempty
                        }
    x `mappend` y   = Domain
                        { _name     = _name x       <> _name y
                        , _arch     = _arch x       <> _arch y
                        , _memory   = _memory x     <> _memory y
                        , _domVCpu  = _domVCpu x    <> _domVCpu y
                        , _cdrom    = _cdrom x      <> _cdrom y
                        , _volume   = _volume x     <> _volume y
                        , _bridge   = _bridge x     <> _bridge y
                        , _domIp    = _domIp x      <> _domIp y
                        }

---- | Generic query for '_arch' working on a /partial/ xml tree.
archXml :: GenericRecQ (Endo Domain, Bool)
--archXml             = endQ (fromParser . parseArch . T.pack)
archXml             = endParserQ (parseArch . T.pack)
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
bridgeXml           = endParserQ (parseInterface . T.pack)
                        `extRecL` pureQ (attrN "bridge")

-- | Generic query for 'IP' inside 'Domain' working on a /partial/ xml tree.
domIpXml :: GenericRecQ (Endo Domain, Bool)
domIpXml            = endParserQ (parseIP . T.pack)
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
                          sourceFileXml (Endo . set . filePath)
  | elN "disk" x && elAttrQN (qn "device") "disk" x  = next $
                          sourceDevXml (\y -> Endo $ modify (volDisk y :))
  | otherwise           = stop mempty
  where
    volDisk :: String -> Volume
    volDisk t       = set (filePath t ) mempty

-- | Generic query for 'Domain' working on a libvirt @domain@ xml.
domainXml :: GenericRecQ (Endo Domain, Bool)
domainXml           = mkRecL dom `extRecL` pureQ (elN "domain")
  where
    dom :: Element -> RecB (Endo Domain)
    dom x
      | elN "name"    x = next $ endParserQ (parseName . onlyTextT)
      | elN "os"      x = next archXml
      | elN "memory"  x = next $ endParserQ (parseSize . onlyTextT)
      | elN "vcpu"    x = next $ endParserQ (parseVCpu . onlyTextT)
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
initDomain f        = modifyM initVols . readDomainXml
  where
    -- | I need that type-signature, otherwise 'Traversable' constraint of
    -- 'mapM' can't be solved.
    initVols :: [Volume] -> m [Volume]
    initVols        = mapM $ \v -> do
                        let p = fromJust . getAlt . _volPath $ v
                        vx <- f p
                        return (v <> readVolumeXml vx)

-- $utils

-- | Use a function (usually, result of parsing something with 'parseOnly') to
-- construct a final 'endQ' generic query. Note, that if i'll take 'Parser b'
-- here instead of 's -> Either e b', i can't make 'endParserQ' to work on any
-- input, like now. Also with current type i may use 'endOfInput' in specific
-- type parsers, because they're evaluated till the end with 'parseOnly' in
-- their specific functions.
endParserQ :: (Typeable s, Typeable b, Data a) =>
              (s -> Either e b) -> GenericRecQ (Endo a, Bool)
endParserQ p        = endQ $ either mempty (Endo . set) . p

-- | Construct a proper 'Monoid' from a 'FilePath'.
filePath :: String -> Alt Maybe F.FilePath
filePath            = Alt . Just . F.decodeString

