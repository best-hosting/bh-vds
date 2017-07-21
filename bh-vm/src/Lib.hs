{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  where

import Prelude      hiding (FilePath)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Options.Applicative
import Control.Applicative
import Text.XML.Light
import Data.Generics
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Data.String
import Control.Arrow
import Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as F (empty)

import Sgf.Data.Generics.Aliases
import Sgf.Data.Generics.Schemes
import Sgf.Text.XML.Light.Proc


infixr 3 *|
(*|) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*|)                = uncurry (***) . (mappend *** (||))

infixr 3 *&
(*&) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*&)                = uncurry (***) . (mappend *** (&&))


infixr 4 <*|>
(<*|>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*|>)              = liftA2 (*|)

infixr 4 <*&>
(<*&>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*&>)              = liftA2 (*&)


-- | Make 'QName' with only 'qName' set.
qn :: String -> QName
qn s            = QName {qName = s, qURI = Nothing, qPrefix = Nothing}


{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}

class FromXML a where
    fromXML :: [Content] -> a

newtype Name        = Name T.Text
  deriving (Show, Typeable, Data, Eq, Monoid)

newtype Size        = Size Integer
  deriving (Show, Typeable, Data, Eq, Ord, Num)

data Pool           = Pool {_poolName :: Name}
  deriving (Show, Typeable, Data, Eq)
defPool :: Pool
defPool             = Pool {_poolName = Name ""}

data Volume         = Volume
                        { _volName  :: Name
                        , _volSize  :: Size
                        , _volPath  :: FilePath
                        , _pool     :: Pool
                        }
  deriving (Show, Typeable, Data, Eq)
defVolume :: Volume
defVolume           = Volume
                        { _volName  = Name ""
                        , _volSize  = Size 0
                        , _volPath  = F.empty
                        , _pool     = defPool
                        }

volNameT :: GenericQ (Name -> Name)
volNameT            = mkT . const . Name . everythingRecBut mappend volNameRQ

volName :: GenericQ (Endo Volume)
volName             = setName <$> everythingRecBut mappend volNameRQ
  where setName xs  = Endo (\vol -> vol{_volName = Name xs})

volNameRQ :: GenericRecQ (T.Text, Bool)
volNameRQ           = end ""  `extRecS` lastQ (T.pack . onlyText')
    `extRecL` elN "name"
    `extRecL` elN "volume"

volSizeT :: GenericQ (Size -> Size)
volSizeT            = const . Size . read . everythingRecBut mappend volSizeRQ

volSize :: GenericQ (Endo Volume)
volSize             = setSize <$> everythingRecBut mappend volSizeRQ
  where setSize x   = Endo (\vol -> vol{_volSize = Size (read x)})

volSizeRQ :: GenericRecQ (String, Bool)
volSizeRQ           = end ""  `extRecS` lastQ onlyText'
    `extRecL` elN "capacity"
    `extRecL` elN "volume"

-- v2

volInitRQ :: GenericRecQ (Endo Volume, Bool)
volInitRQ           = mkRecL vol2 `extRecL` elN "volume"

vol2 :: Element -> ((Endo Volume, Bool), GenericRecQ (Endo Volume, Bool))
vol2 x
  | qName (elName x) == "capacity"  = ((mempty, False), volSize2)
  | qName (elName x) == "name"      = ((mempty, False), volName2)
  | otherwise                       = stop mempty

volName2 :: GenericRecQ (Endo Volume, Bool)
volName2          = end mempty  `extRecS` lastQ (setName . onlyText')
  where setName xs  = Endo (\vol -> vol{_volName = Name (T.pack xs) <> _volName vol})

volSize2 :: GenericRecQ (Endo Volume, Bool)
volSize2          = end mempty  `extRecS` lastQ (setSize . onlyText')
  where setSize x  = Endo (\vol -> vol{_volSize = Size (read x)})


newtype IP          = IP T.Text
  deriving (Show, Typeable, Data, Eq)
defIP :: IP
defIP               = IP "0.0.0.0"

newtype Interface   = Interface T.Text
  deriving (Show, Typeable, Data, Eq)
defInterface :: Interface
defInterface        = Interface "lo"

-- FIXME: vm may have several volumes.
data Domain         = Domain
                        { _name         :: Name
                        , _arch         :: T.Text
                        , _memory       :: Size
                        , _vcpu         :: Int
                        , _cdrom        :: Maybe FilePath
                        , _domIp        :: IP
                        , _bridge       :: Interface
                        , _volume       :: Volume
                        }
  deriving (Show, Typeable, Data, Eq)
defDomain :: Domain
defDomain           = Domain
                        { _name         = Name ""
                        , _arch         = ""
                        , _memory       = Size 0
                        , _vcpu         = 0
                        , _cdrom        = Nothing
                        , _domIp        = defIP
                        , _bridge       = defInterface
                        , _volume       = defVolume
                        }

name :: GenericQ (Endo Domain)
name                = setName <$> everythingRecBut mappend nameRQ
  where setName xs  = Endo (\dom -> dom{_name = Name xs})
nameRQ :: GenericRecQ (T.Text, Bool)
nameRQ              = end "" `extRecS` lastQ (T.pack . onlyText')
    `extRecL` elN "name"
    `extRecL` elN "domain"

arch :: GenericQ (Endo Domain)
arch                = setName <$> everythingRecBut mappend archRQ
  where setName xs  = Endo (\dom -> dom{_arch = xs})
archRQ :: GenericRecQ (T.Text, Bool)
archRQ              = end "" `extRecS` lastQ T.pack `extRecL` attrN "arch"
    `extRecL` elN "type"
    `extRecL` elN "os"
    `extRecL` elN "domain"

memory :: GenericQ (Endo Domain)
memory              = setName <$> everythingRecBut mappend memoryRQ
  where setName xs  = Endo (\dom -> dom{_memory = Size (read xs)})
memoryRQ ::GenericRecQ (String, Bool)
memoryRQ            = end "" `extRecS` lastQ onlyText'
    `extRecL` elN "memory"
    `extRecL` elN "domain"

vcpu :: GenericQ (Endo Domain)
vcpu                = setName . read <$> everythingRecBut mappend vcpuRQ
  where setName xs  = Endo (\dom -> dom{_vcpu = xs})
vcpuRQ :: GenericRecQ (String, Bool)
vcpuRQ              = end "" `extRecS` lastQ onlyText'
    `extRecL` elN "vcpu"
    `extRecL` elN "domain"

cdrom :: GenericQ (Endo Domain)
cdrom               = setName <$> everythingRecBut mappend cdromRQ
  where setName xs  = Endo (\dom -> dom{_cdrom = xs})
cdromRQ :: GenericRecQ (Maybe FilePath, Bool)
cdromRQ             = end Nothing `extRecS` lastQ (Just . decodeString) `extRecL` attrN "file"
    `extRecL` elN "source"
    `extRecL` (elN "disk" <*|> elAttrQN (qn "device") "cdrom")
    --`extRecL` elN "disk"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

bridge :: Data a => a -> Endo Domain
bridge              = setName <$> everythingRecBut mappend bridgeRQ
  where setName xs  = Endo (\int -> int{_bridge = Interface xs})
bridgeRQ :: GenericRecQ (T.Text, Bool)
bridgeRQ            = end "" `extRecS` lastQ T.pack `extRecL` attrN "bridge"
    `extRecL` elN "source"
    `extRecL` elN "interface"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

domIp :: Data a => a -> Endo Domain
domIp               = setName <$> everythingRecBut mappend domIpRQ
  where setName xs  = Endo (\int -> int{_domIp = IP xs})
domIpRQ :: GenericRecQ (T.Text, Bool)
domIpRQ             = end "" `extRecS` lastQ T.pack `extRecL` attrN "value"
    `extRecL` (elN "parameter" <*|> elAttrQN (qn "name") "IP")
    `extRecL` elN "filterref"
    `extRecL` elN "interface"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

domPath :: GenericQ (Endo Domain)
domPath             = setName <$> everythingRecBut mappend domPathRQ
  where setName xs  = Endo (\dom -> dom{_volume = (\vol -> vol{_volPath = xs}) (_volume dom)})
domPathRQ :: GenericRecQ (FilePath, Bool)
domPathRQ           = end "" `extRecS` lastQ decodeString `extRecL` attrN "dev"
    `extRecL` elN "source"
    `extRecL` (elN "disk" <*|> elAttrQN (qn "device") "disk")
    `extRecL` elN "devices"
    `extRecL` elN "domain"

volume :: Endo Volume -> Endo Domain
volume v            = Endo (\dom -> dom{_volume = appEndo v (_volume dom)})

-- v2

pureQ :: Monoid r => (a -> Bool) -> a -> (r, Bool)
pureQ p x           = (mempty, p x)

stop :: r -> ((r, Bool), GenericRecQ (r, Bool))
stop d              = ((d, True), undefined)

pass :: Monoid r => GenericRecQ (r, Bool) -> ((r, Bool), GenericRecQ (r, Bool))
pass cont           = ((mempty, False), cont)


domInitRQ :: GenericRecQ (Endo Domain, Bool)
domInitRQ           = mkRecL dom2 `extRecL` elN "domain"

dom2 :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
dom2 x
  | qName (elName x) == "name"      = ((mempty, False), name2)
  | qName (elName x) == "os"        = ((mempty, False), arch2)
  | qName (elName x) == "memory"    = ((mempty, False), memory2)
  | qName (elName x) == "vcpu"      = pass vcpu2
  -- | qName (elName x) == "devices"   = pass cdrom2
  | qName (elName x) == "devices"   = pass (mkRecL domDevices2)
  | otherwise                       = stop mempty

name2 :: GenericRecQ (Endo Domain, Bool)
name2               = lastQ2 (setName . onlyText')
  where setName xs  = Endo (\dom -> dom{_name = Name (T.pack xs)})

arch2 :: GenericRecQ (Endo Domain, Bool)
arch2              = end mempty `extRecS` lastQ setName `extRecL` attrN "arch"
    `extRecL` elN "type"
  where setName xs  = Endo (\dom -> dom{_arch = T.pack xs})

memory2 :: GenericRecQ (Endo Domain, Bool)
memory2            = end mempty `extRecS` lastQ (setName . onlyText')
  where setName xs  = Endo (\dom -> dom{_memory = Size (read xs)})

vcpu2 :: GenericRecQ (Endo Domain, Bool)
vcpu2               = end mempty `extRecS` lastQ (setName . onlyText')
  where setName xs  = Endo (\dom -> dom{_vcpu = read xs})

domDevices2 :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domDevices2 x
  -- | qName (elName x) == "interface" = pass bridge2
  | qName (elName x) == "interface" = pass (mkRecL domInterfaces2)
  | elN2 "disk" x && elAttrQN2 (qn "device") "cdrom" x  = pass cdrom2
  | elN2 "disk" x && elAttrQN2 (qn "device") "disk" x  = pass domPath2
  -- | qName (elName x) == "disk"  = pass cdrom2
  | otherwise   = stop mempty

cdrom2 :: GenericRecQ (Endo Domain, Bool)
cdrom2              = lastQ2 (setName . decodeString) `extRecL` attrN "file"
    `extRecL` elN "source"
    --`extRecL` (elN "disk" <*|> elAttrQN (qn "device") "cdrom")
  where setName xs  = Endo (\dom -> dom{_cdrom = Just xs})

-- | FIXME: Rename 'domPath' to 'domDisk' .
domPath2 :: GenericRecQ (Endo Domain, Bool)
domPath2           = lastQ2 setName `extRecL` attrN "dev"
    `extRecL` elN "source"
  where setName xs  = Endo (\dom -> dom{_volume = (\vol -> vol{_volPath = decodeString xs}) (_volume dom)})

domInterfaces2 :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domInterfaces2 x
  | qName (elName x) == "source"    = pass bridge2
  | qName (elName x) == "filterref" = pass domIp2
  | otherwise   = stop mempty

bridge2 :: GenericRecQ (Endo Domain, Bool)
bridge2            = lastQ2 setName `extRecL` attrN "bridge"
    --`extRecL` elN "source"
  where setName xs  = Endo (\int -> int{_bridge = Interface (T.pack xs)})

domIp2 :: GenericRecQ (Endo Domain, Bool)
domIp2              = lastQ2 setName `extRecL` attrN "value"
    `extRecL` (elN "parameter" <*|> elAttrQN (qn "name") "IP")
  where setName xs  = Endo (\int -> int{_domIp = IP (T.pack xs)})



{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}


dom1 :: Domain
dom1    = Domain {_name = Name "{{name}}", _arch = "{{arch}}", _memory = Size 128, _vcpu = 2, _cdrom = Just (decodeString "/var/virt/mini-ubuntu-16.04-i386.iso"), _domIp = IP "{{ip}}", _bridge = Interface "{{bridge}}", _volume = Volume {_volName = Name "vm-{{name}}", _volSize = Size 2345, _volPath = decodeString "/dev/zero/", _pool = Pool {_poolName = Name ""}}}

-- T.readFile "../volume.xml" >>= return . (\x -> gmapT (id `extT` volNameT x `extT` volSizeT x) defVolume) . parseXML
main :: IO ()
main                = do
    cv <- parseXML <$> T.readFile "../vol.xml"
    cd <- parseXML <$> T.readFile "../dom.xml"
    let vl  = volSize cv <> volName cv
        vl2 = everythingRecBut mappend volInitRQ cv
        dom =       name cd
                <>  arch cd
                <> memory cd
                <> vcpu cd
                <> cdrom cd
                <> bridge cd
                <> domIp cd
                <> volume vl
                <> domPath cd
        dom2 = everythingRecBut mappend domInitRQ cd
        d  = ($ defDomain) . appEndo $ dom
        d2 = ($ defDomain) . appEndo $ (dom2 <> volume vl2)
    print d
    print (decodeString "/dev/zero")
    print d2
    print (d == d2)

