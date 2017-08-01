{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    )
  where

import Data.Monoid
import Data.Generics
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
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

-- | Type for libvirt storage volume.
data Volume         = Volume
                        { _volName  :: Name         -- ^ Volume @name@.
                        , _volSize  :: Size         -- ^ Volume @capacity@.
                        , _volPath  :: F.FilePath   -- ^ Volume @target -> path@.
                        , _pool     :: Pool         -- ^ Volume pool.
                        }
  deriving (Show, Typeable, Data, Eq)

instance Monoid Volume where
    mempty          = Volume
                        { _volName  = mempty
                        , _volSize  = mempty
                        , _volPath  = F.empty
                        , _pool     = mempty
                        }
    x `mappend` y   = Volume
                        { _volName  = _volName x <> _volName y
                        , _volSize  = _volSize x <> _volSize y
                        , _volPath  = _volPath x <> _volPath y
                        , _pool     = _pool x    <> _pool y
                        }

sumNames :: Name -> Name -> Name
sumNames (Name (Last mx)) (Name (Last my)) = Name (Last (mx <> my))

fromParser :: (Typeable b, Data a) => Either e b -> Endo a
fromParser          = either mempty (Endo . set)

-- FIXME: 'volNameXml' and 'volSizeXml' are identical, except for parser. Join
-- them? The same for 'nameXml', 'memoryXml', 'vcpuXml'.
-- | Generic query for '_volName' working on a /partial/ xml tree.
volNameXml :: GenericRecQ (Endo Volume, Bool)
volNameXml      = endQ $ fromParser . parseName . onlyTextT

-- | Generic query for '_volSize' working on a /partial/ xml tree.
volSizeXml :: GenericRecQ (Endo Volume, Bool)
volSizeXml      = endQ $ fromParser . parseSize . onlyTextT

-- | Generic query for 'Volume' working on a libvirt storage @volume@ xml.
volumeXml :: GenericRecQ (Endo Volume, Bool)
volumeXml       = mkRecL vol `extRecL` pureQ (elN "volume")
  where
    vol :: Element -> RecB (Endo Volume)
    vol x
      | elN "capacity" x    = next volSizeXml
      | elN "name"     x    = next volNameXml
      | otherwise           = stop mempty

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
newtype IP          = IP (Last T.Text)
  deriving (Show, Typeable, Data, Eq, Monoid)

-- FIXME: Proper IP parsing.
-- | Parser for 'IP'.
parseIP :: T.Text -> Either String IP
parseIP             = Right . IP . Last . Just

-- | Type for libvirt @domain@.
data Domain         = Domain
                        { _name     :: Name
                        , _arch     :: Arch
                        , _memory   :: Size
                        , _domVCpu  :: VCpu
                        , _cdrom    :: Maybe F.FilePath
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
                        , _cdrom    = Nothing
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

-- | Generic query for '_name' working on a /partial/ xml tree.
nameXml :: GenericRecQ (Endo Domain, Bool)
nameXml         = endQ (fromParser . parseName . onlyTextT)

-- | Generic query for '_arch' working on a /partial/ xml tree.
archXml :: GenericRecQ (Endo Domain, Bool)
archXml         = endQ (fromParser . parseArch . T.pack)
                        `extRecL` pureQ (attrN "arch")
                        `extRecL` pureQ (elN "type")

-- | Generic query for '_memory' working on a /partial/ xml tree.
memoryXml :: GenericRecQ (Endo Domain, Bool)
memoryXml       = endQ (fromParser . parseSize . onlyTextT)

-- | Generic query for '_vcpu' working on a /partial/ xml tree.
vcpuXml :: GenericRecQ (Endo Domain, Bool)
vcpuXml         = endQ (fromParser . parseVCpu . onlyTextT)

-- | Generic query for '_cdrom' working on a /partial/ xml tree.
cdromXml :: GenericRecQ (Endo Domain, Bool)
cdromXml        = endQ (Endo . set . Just . F.decodeString)
                    `extRecL` pureQ (attrN "file")
                    `extRecL` pureQ (elN "source")

-- FIXME: F.decodeString "/a/b/c" `mappend` mempty == FilePath "/a/b/c/" .
-- | Generic query for '_volPath' in 'Volume' inside 'Domain' working on a
-- /partial/ xml tree. Each found '_volPath' is decoded as /separate/
-- 'Volume'.
domDiskXml :: GenericRecQ (Endo Domain, Bool)
domDiskXml      = endQ (\x -> Endo $ modify (volDisk x :))
                    `extRecL` pureQ (attrN "dev")
                    `extRecL` pureQ (elN "source")
  where
    volDisk :: String -> Volume
    volDisk x   = set (F.decodeString x) mempty

-- | Generic query for 'Interface' inside 'Domain' working on a /partial/ xml
-- tree.
bridgeXml :: GenericRecQ (Endo Domain, Bool)
bridgeXml       = endQ (fromParser . parseInterface . T.pack)
                    `extRecL` pureQ (attrN "bridge")

-- | Generic query for 'IP' inside 'Domain' working on a /partial/ xml tree.
domIpXml :: GenericRecQ (Endo Domain, Bool)
domIpXml        = endQ (fromParser . parseIP . T.pack)
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
-- in libvirt domain xml tree (works on /partial/ xml tree).
domDevices :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domDevices x
  | elN "interface" x   = next (mkRecL domInterfaces)
  | elN "disk" x && elAttrQN (qn "device") "cdrom" x
                        = next cdromXml
  | elN "disk" x && elAttrQN (qn "device") "disk" x
                        = next domDiskXml
  | otherwise           = stop mempty

-- | Generic query for 'Domain' working on a libvirt @domain@ xml.
domainXml :: GenericRecQ (Endo Domain, Bool)
domainXml           = mkRecL dom `extRecL` pureQ (elN "domain")
  where
    dom :: Element -> RecB (Endo Domain)
    dom x
      | elN "name"    x = next nameXml
      | elN "os"      x = next archXml
      | elN "memory"  x = next memoryXml
      | elN "vcpu"    x = next vcpuXml
      | elN "devices" x = next (mkRecL domDevices)
      | otherwise       = stop mempty

-- | Parse 'Domain' from an 'XmlSource' containing libvirt @domain@ xml.
readDomainXml :: X.XmlSource s => s -> Domain
readDomainXml       = ($ mempty) . appEndo
                        . everythingRecBut mappend domainXml
                        . parseXML

