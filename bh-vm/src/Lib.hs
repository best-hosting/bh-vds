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


infixr 4 <&&>
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>)              = liftA2 (&&)

infixr 4 <*|>
(<*|>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*|>)              = liftA2 (*|)

infixr 4 <*&>
(<*&>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*&>)              = liftA2 (*&)



{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}

type Rec r          = (r, GenericRecQ r)
type RecB r         = Rec (r, Bool)

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

volume :: GenericRecQ (Endo Volume, Bool)
volume              = mkRecL vol `extRecL` pureQ (elN2 "volume")
  where
    vol :: Element -> RecB (Endo Volume)
    vol x
      | qName (elName x) == "capacity"  = next volSize
      | qName (elName x) == "name"      = next volName
      | otherwise                       = stop mempty

volName :: GenericRecQ (Endo Volume, Bool)
volName             = endQ (setName . onlyText')
  where setName xs  = Endo $ \vol ->
                        vol{_volName = Name (T.pack xs) <> _volName vol}

volSize :: GenericRecQ (Endo Volume, Bool)
volSize             = endQ (setSize . onlyText')
  where setSize x   = Endo $ \vol -> vol{_volSize = Size (read x)}


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

domain :: GenericRecQ (Endo Domain, Bool)
domain              = mkRecL dom `extRecL` pureQ (elN2 "domain")
  where
    dom :: Element -> RecB (Endo Domain)
    dom x
      | qName (elName x) == "name"      = next name
      | qName (elName x) == "os"        = next arch
      | qName (elName x) == "memory"    = next memory
      | qName (elName x) == "vcpu"      = next vcpu
      | qName (elName x) == "devices"   = next (mkRecL domDevices)
      | otherwise                       = stop mempty

name :: GenericRecQ (Endo Domain, Bool)
name                = endQ (setName . onlyText')
  where setName xs  = Endo $ \dom -> dom{_name = Name (T.pack xs)}

arch :: GenericRecQ (Endo Domain, Bool)
arch                = endQ setName `extRecL` pureQ (attrN2 "arch")
                        `extRecL` pureQ (elN2 "type")
  where setName xs  = Endo $ \dom -> dom{_arch = T.pack xs}

memory :: GenericRecQ (Endo Domain, Bool)
memory              = endQ (setName . onlyText')
  where setName xs  = Endo $ \dom -> dom{_memory = Size (read xs)}

vcpu :: GenericRecQ (Endo Domain, Bool)
vcpu                = endQ (setName . onlyText')
  where setName xs  = Endo $ \dom -> dom{_vcpu = read xs}

domDevices :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domDevices x
  | qName (elName x) == "interface"
                    = next (mkRecL domInterfaces)
  | elN2 "disk" x && elAttrQN2 (qn "device") "cdrom" x
                    = next cdrom
  | elN2 "disk" x && elAttrQN2 (qn "device") "disk" x
                    = next domPath
  | otherwise       = stop mempty

cdrom :: GenericRecQ (Endo Domain, Bool)
cdrom               = endQ (setName . decodeString)
                        `extRecL` pureQ (attrN2 "file")
                        `extRecL` pureQ (elN2 "source")
  where setName xs  = Endo $ \dom -> dom{_cdrom = Just xs}

-- | FIXME: Rename 'domPath' to 'domDisk' .
domPath :: GenericRecQ (Endo Domain, Bool)
domPath             = endQ setName `extRecL` pureQ (attrN2 "dev")
                        `extRecL` pureQ (elN2 "source")
  where setName xs  = Endo $ \dom ->
                        dom{_volume = (\vol ->
                        vol{_volPath = decodeString xs}) (_volume dom)}

domInterfaces :: Element -> ((Endo Domain, Bool), GenericRecQ (Endo Domain, Bool))
domInterfaces x
  | qName (elName x) == "source"    = next bridge
  | qName (elName x) == "filterref" = next domIp
  | otherwise                       = stop mempty

bridge :: GenericRecQ (Endo Domain, Bool)
bridge              = endQ setName `extRecL` pureQ (attrN2 "bridge")
  where setName xs  = Endo $ \int -> int{_bridge = Interface (T.pack xs)}

domIp :: GenericRecQ (Endo Domain, Bool)
domIp               = endQ setName `extRecL` pureQ (attrN2 "value")
    --`extRecL` (pureQ (elN2 "parameter") <*|> pureQ (elAttrQN2 (qn "name") "IP"))
    `extRecL` (pureQ (elN2 "parameter" <&&> elAttrQN2 (qn "name") "IP"))
  where setName xs  = Endo $ \int -> int{_domIp = IP (T.pack xs)}

domVolume :: Endo Volume -> Endo Domain
domVolume v         = Endo $ \dom -> dom{_volume = appEndo v (_volume dom)}



{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}

-- T.readFile "../volume.xml" >>= return . (\x -> gmapT (id `extT` volNameT x `extT` volSizeT x) defVolume) . parseXML
main :: IO ()
main                = do
    cv <- parseXML <$> T.readFile "../vol.xml"
    cd <- parseXML <$> T.readFile "../dom.xml"
    let vol = everythingRecBut mappend volume cv
        dom = everythingRecBut mappend domain cd
        d = ($ defDomain) . appEndo $ (dom <> domVolume vol)
    print (decodeString "/dev/zero")
    print d

