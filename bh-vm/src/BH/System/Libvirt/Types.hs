{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BH.System.Libvirt.Types
    (
    SystemConf (..)
    , PlanConf (..)
    , OsConf (..)

    , IPMap

    , P
    , runP
    , PState (..)
    , Config (..)
    , defConfig
    )
  where

import Data.Maybe
import Prelude      hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Control.Monad.Reader
import Control.Monad.Except
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Yaml.Aeson

import System.Libvirt.Types
import System.Libvirt.Template


type P a            = StateT PState (ReaderT Config (ExceptT VmError IO)) a

type IPMap          = M.Map IP (S.Set Domain)

data PState         = PState {domain :: Domain, ipMap :: IPMap}
  deriving (Show)
defPState :: PState
defPState           = PState {domain = mempty, ipMap = mempty}

runP :: Config -> P () -> IO ()
runP cf mx          = do
    r <- runExceptT . flip runReaderT cf . flip runStateT defPState $ mx
    liftIO $ case r of
      Left (XmlGenError pe)     -> printParserError loadFile pe >>= putStrLn
      Left (YamlParseError f pe) -> putStrLn $
        "Yaml parse error in file '" ++ F.encodeString f ++ "':\n"
            ++ prettyPrintParseException pe
      Left (LibvirtError er)    -> T.putStrLn er
      Left (UnknownError t)     -> print $ "Unknown Error type: " ++ show t
      Right _ -> return ()

-- | Defaults, which are added to all others.
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
                        <*> (parseIPMap <$> o .: "ipmap")
instance ToJSON SystemConf where
    toJSON SystemConf{..}   = object $
                                [ "volume"  .= sysVolume
                                , "domain"  .= sysDomain
                                , "ipmap"   .= writeIPMap sysIpMap
                                ]

parseIPMap :: M.Map T.Text a -> M.Map IP a
parseIPMap m    = M.foldrWithKey go M.empty m
  where
    go :: T.Text -> a -> M.Map IP a -> M.Map IP a
    go x d zm   = either (const zm) (\y -> M.insert y d zm)
                    . parseIP $ x
writeIPMap :: M.Map IP a -> M.Map T.Text a
writeIPMap m    = M.foldrWithKey go M.empty m
  where
    go :: IP -> a -> M.Map T.Text a -> M.Map T.Text a
    go x d zm   = M.insert (showt x) d zm

-- | Domain settings required by a plan.
newtype PlanConf  = PlanConf {planDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)
newtype OsConf      = OsConf {osDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)

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

