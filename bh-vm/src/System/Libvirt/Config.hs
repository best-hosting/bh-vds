{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: System.Libvirt.Config
--
-- Library config.

module System.Libvirt.Config
    (
    -- * Main context.
    --
    -- $main
      printVmError
    , P
    , runP
    , PState (..)
    , defPState
    , Config (..)
    , defConfig

    -- * Config files information.
    --
    -- $configs
    , SystemConf (..)
    , PlanConf (..)
    , OsConf (..)
    , mergeConfigs
    )
  where

import           Data.Monoid
import qualified Data.Text                  as T
import           TextShow
import           Control.Monad.Reader
import           Control.Exception
import qualified Filesystem.Path.CurrentOS  as F
import           Control.Monad.State
import           Control.Monad.Managed
import           Data.Yaml.Aeson

import System.Libvirt.Types
import System.Libvirt.Template


-- $main

-- | Pretty print 'VmError'.
printVmError :: MonadIO m => VmError -> m String
printVmError err    = case err of
    XmlGenError pe      -> printParserError loadFile pe
    YamlParseError f pe -> return $
        "Yaml parse error in file '" ++ F.encodeString f ++ "':\n"
            ++ prettyPrintParseException pe
    LibvirtError er     -> return $
        "Libvirt error:\n" ++ er
    IPAlreadyInUse i ds -> return $
        "IP " ++ T.unpack (showt i) ++ " already used by:\n" ++ show ds
    IPNotAvailable i f  -> return $
        "IP " ++ show i ++ " is not available." ++
        " Add it to '" ++ F.encodeString f ++ "' config first."
    NoFreeIPs f         -> return $
        "No free IPs." ++ " Add more to '" ++ F.encodeString f ++ "' config."
    UnknownError t      -> return $ "Unknown Error type: " ++ show t

-- | Main monad.
type P a    = StateT PState (ReaderT Config Managed) a

-- | Run 'P' monad.
runP :: Config -> P () -> IO ()
runP cf mx          = runManaged (runReaderT (evalStateT mx defPState) cf)
                        `catch` \e ->
    case fromException e of
      Just err  -> printVmError err >>= putStrLn
      _         -> do
        putStrLn "Unknown exception: "
        putStrLn (displayException e)

-- | Main state.
data PState         = PState {domain :: Domain, ipMap :: IPMap}
  deriving (Show)
-- | Default 'PState'.
defPState :: PState
defPState           = PState {domain = mempty, ipMap = mempty}

-- | Main readonly config.
data Config         = Config
                        { planConfFile  :: Maybe F.FilePath
                        , sysConfFile   :: F.FilePath
                        , osConfFile    :: Maybe F.FilePath
                        , domTmplFile   :: F.FilePath
                        , volTmplFile   :: F.FilePath
                        , domName       :: Name
                        , domIp         :: Maybe IP
                        }
  deriving (Show)
-- | Default 'Config'.
defConfig :: Config
defConfig           = Config
                        { planConfFile  = Just "../plan.yaml"
                        , sysConfFile   = "../system.yaml"
                        , osConfFile    = Just "../os.yaml"
                        , domTmplFile   = "../dom.xml"
                        , volTmplFile   = "../vol.xml"
                        , domName       = "test"
                        , domIp         = either (const Nothing) Just $
                                            parseIP "1.1.1.1"
                        }

-- $configs

-- | Defaults, which are added to all others values.
data SystemConf     = SystemConf
                        { sysVolume :: Volume
                        , sysDomain :: Domain
                        , sysIpMap  :: IPMap
                        }
  deriving (Show)

instance FromJSON SystemConf where
    parseJSON       = withObject "SystemConf" $ \o -> SystemConf
                        <$> o .: "volume"
                        <*> o .: "domain"
                        <*> o .: "ipmap"
instance ToJSON SystemConf where
    toJSON SystemConf{..}   = object
                                [ "volume"  .= sysVolume
                                , "domain"  .= sysDomain
                                , "ipmap"   .= sysIpMap
                                ]

-- | Domain settings required by a plan.
newtype PlanConf  = PlanConf {planDomain :: Domain}
  deriving (Show, FromJSON, ToJSON, Monoid)

-- | Domain settings required by used OS.
newtype OsConf      = OsConf {osDomain :: Domain}
  deriving (Show, FromJSON, ToJSON, Monoid)

-- | Merge 'SystemConf' into initial 'Domain' value (probably, the result of
-- summing 'PlanConf' and 'OsConf').
mergeConfigs :: Name -> SystemConf -> Domain -> PState
mergeConfigs dn SystemConf{..} d0 =
    -- Note, that 'volName' from 'sysDomain' is appended /directly/ (without
    -- separators) to 'volName' from 'd0'.
    let d = sysDomain <> d0
        d' = d{ name = dn
              , volume = (sysVolume <> volume d)
                            {volName = addVolNames (volume d)}
              }
    -- I'm interested only in IP addresses. List of domains, which use them,
    -- i'll rescan later.
    in  PState{domain = d', ipMap = sysIpMap}
  where
    addVolNames :: Volume -> Name
    addVolNames v   = volName sysVolume +++ dn +++ volName v
    (+++) :: Name -> Name -> Name
    x +++ y         = maybeSep "-" x y

-- | Append 'Monoid' values adding separator between them, if /both/ are not
-- 'mempty'.
maybeSep :: (Eq a, Monoid a) => a -> a -> a -> a
maybeSep s x y
  | x == mempty     = y
  | y == mempty     = x
  | otherwise       = x <> s <> y

