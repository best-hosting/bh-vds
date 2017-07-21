{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

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

infixr 4 <*|>
(<*|>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*|>)              = liftA2 (*|)


-- | Make 'QName' with only 'qName' set.
qn :: String -> QName
qn s            = QName {qName = s, qURI = Nothing, qPrefix = Nothing}


{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}

class FromXML a where
    fromXML :: [Content] -> a

data Volume         = Volume
                        { _volName  :: T.Text
                        , _volSize  :: Integer
                        , _volPath  :: FilePath
                        , _pool     :: T.Text
                        }
  deriving (Show, Typeable, Data)
defVolume :: Volume
defVolume           = Volume
                        { _volName  = ""
                        , _volSize  = 0
                        , _volPath  = F.empty
                        , _pool     = ""
                        }

volName :: GenericQ (Endo Volume)
volName             = setName <$> everythingRecBut mappend volNameRQ
  where setName xs  = Endo (\vol -> vol{_volName = xs})
volNameRQ :: GenericRecQ (T.Text, Bool)
volNameRQ           = end ""  `extRecS` lastQ (T.pack . onlyText')
    `extRecL` elN "name"
    `extRecL` elN "volume"

volSize :: GenericQ (Endo Volume)
volSize             = setSize <$> everythingRecBut mappend volSizeRQ
  where setSize x   = Endo (\vol -> vol{_volSize = read x})
volSizeRQ :: GenericRecQ (String, Bool)
volSizeRQ           = end ""  `extRecS` lastQ onlyText'
    `extRecL` elN "capacity"
    `extRecL` elN "volume"

data Interface      = Interface
                        { _ip       :: T.Text
                        , _bridge   :: T.Text
                        }
  deriving (Show, Typeable, Data)
defInterface :: Interface
defInterface        = Interface
                        { _ip       = ""
                        , _bridge   = ""
                        }

bridge :: Data a => a -> Endo Interface
bridge              = setName <$> everythingRecBut mappend bridgeRQ
  where setName xs  = Endo (\int -> int{_bridge = xs})
bridgeRQ :: GenericRecQ (T.Text, Bool)
bridgeRQ            = end "" `extRecS` lastQ T.pack `extRecL` attrN "bridge"
    `extRecL` elN "source"
    `extRecL` elN "interface"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

ip :: Data a => a -> Endo Interface
ip                  = setName <$> everythingRecBut mappend ipRQ
  where setName xs  = Endo (\int -> int{_ip = xs})
ipRQ :: GenericRecQ (T.Text, Bool)
ipRQ                = end "" `extRecS` lastQ T.pack `extRecL` attrN "value"
    `extRecL` (elN "parameter" <*|> elAttrQN (qn "name") "IP")
    `extRecL` elN "filterref"
    `extRecL` elN "interface"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

-- FIXME: vm may have several volumes.
data Domain         = Domain
                        { _name         :: T.Text
                        , _arch         :: T.Text
                        , _memory       :: Int
                        , _vcpu         :: Int
                        , _cdrom        :: Maybe FilePath
                        , _interface    :: Interface
                        , _volume       :: Volume
                        }
  deriving (Show, Typeable, Data)
defDomain :: Domain
defDomain           = Domain
                        { _name         = ""
                        , _arch         = ""
                        , _memory       = 0
                        , _vcpu         = 0
                        , _cdrom        = Nothing
                        , _interface    = defInterface
                        , _volume       = defVolume
                        }

name :: GenericQ (Endo Domain)
name                = setName <$> everythingRecBut mappend nameRQ
  where setName xs  = Endo (\dom -> dom{_name = xs})
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
memory              = setName . read <$> everythingRecBut mappend memoryRQ
  where setName xs  = Endo (\dom -> dom{_memory = xs})
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
    --`extRecL` (elN "disk" <*|> elAttrQN (qn "device") "cdrom")
    `extRecL` elN "disk"
    `extRecL` elN "devices"
    `extRecL` elN "domain"

domPath :: GenericQ (Endo Domain)
{-domPath x           = let v = setName (everythingRecBut mappend domPathRQ x)
                      in  Endo (\dom -> dom{_volume = appEndo v (_volume dom)})
  where setName xs  = Endo (\vol -> vol{_volPath = xs})-}
domPath             = setName <$> everythingRecBut mappend domPathRQ
  where setName xs  = Endo (\dom -> dom{_volume = (\vol -> vol{_volPath = xs}) (_volume dom)})
domPathRQ :: GenericRecQ (FilePath, Bool)
domPathRQ           = end "" `extRecS` lastQ decodeString `extRecL` attrN "dev"
    `extRecL` elN "source"
    `extRecL` (elN "disk" <*|> elAttrQN (qn "device") "disk")
    `extRecL` elN "devices"
    `extRecL` elN "domain"

domPathRQ2 :: GenericRecQ (String, Bool)
domPathRQ2           = end "" `extRecS` lastQ id `extRecL` attrN "dev"
    `extRecL` elN "source"
    `extRecL` (elN "disk" <*|> elAttrQN (qn "device") "disk")
    `extRecL` elN "devices"
    `extRecL` elN "domain"

volume :: Endo Volume -> Endo Domain
volume v            = Endo (\dom -> dom{_volume = appEndo v (_volume dom)})

interface :: Endo Interface -> Endo Domain
interface v         = Endo (\dom -> dom{_interface = appEndo v (_interface dom)})

{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}


dom1 :: Domain
dom1 = Domain {_name = "{{name}}", _arch = "{{arch}}", _memory = 128, _vcpu = 2, _cdrom = Just (decodeString "/var/virt/mini-ubuntu-16.04-i386.iso"), _interface = Interface {_ip = "{{ip}}", _bridge = "{{bridge}}"}, _volume = Volume {_volName = "vm-{{name}}", _volSize = 2345, _volPath = decodeString "/dev/zero", _pool = ""}}

main :: IO ()
main                = do
    cv <- parseXML <$> T.readFile "../vol.xml"
    cd <- parseXML <$> T.readFile "../dom.xml"
    let vl  = volSize cv <> volName cv
        int = ip cd <> bridge cd
        dom = name cd <> arch cd <> memory cd <> vcpu cd <> cdrom cd <> volume vl <> interface int <> domPath cd
    print . ($ defDomain) . appEndo $ dom
    print (decodeString "/dev/zero")

