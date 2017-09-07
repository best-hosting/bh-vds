{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: System.Libvirt.Config
--

module System.Libvirt.Config
    (
    -- * Main.
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
    )
  where

import qualified Data.Text                  as T
import           TextShow
import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Filesystem.Path.CurrentOS  as F
import           Control.Monad.State
import           Control.Monad.Managed
import           Data.Yaml.Aeson

import System.Libvirt.Types
import System.Libvirt.IP
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
    IPNotAvailable i er -> return $
        "IP " ++ show i ++ " is not available." ++ er
    NoFreeIPs er        -> return $ "No free IPs. " ++ show er
    UnknownError t      -> return $ "Unknown Error type: " ++ show t

-- | Main monad.
type P a    = StateT PState (ReaderT Config (ExceptT VmError Managed)) a

-- | Run 'P' monad.
runP :: Config -> P () -> IO ()
runP cf mx          = runManaged $ do
    r <- runExceptT . flip runReaderT cf . flip runStateT defPState $ mx
    liftIO $ case r of
      Left e  -> printVmError e >>= putStrLn
      Right _ -> return ()

-- | Main state.
data PState         = PState {domain :: Domain, ipMap :: IPMap}
  deriving (Show)
-- | Default 'PState'.
defPState :: PState
defPState           = PState {domain = mempty, ipMap = mempty}

-- | Main readonly config.
data Config         = Config
                        { planConfFile  :: F.FilePath
                        , sysConfFile   :: F.FilePath
                        , osConfFile    :: F.FilePath
                        , domTmplFile   :: F.FilePath
                        , volTmplFile   :: F.FilePath
                        , domName       :: Name
                        , domIp         :: Maybe IP
                        }
  deriving (Show)
-- | Default 'Config'.
defConfig :: Config
defConfig           = Config
                        { planConfFile  = "../plan.yaml"
                        , sysConfFile   = "../system.yaml"
                        , osConfFile    = "../os.yaml"
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
    toJSON SystemConf{..}   = object $
                                [ "volume"  .= sysVolume
                                , "domain"  .= sysDomain
                                , "ipmap"   .= sysIpMap
                                ]

-- | Domain settings required by a plan.
newtype PlanConf  = PlanConf {planDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)

-- | Domain settings required by used OS.
newtype OsConf      = OsConf {osDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)

