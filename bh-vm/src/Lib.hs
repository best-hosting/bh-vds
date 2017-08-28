{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib
  where

import Data.Maybe
import Prelude      hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Options.Applicative
import Control.Applicative
import TextShow
import Control.Monad.Reader
import Data.Monoid
import qualified Data.String as S
import Control.Monad.Except
import Filesystem.Path.CurrentOS ((</>), (<.>), basename)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Map as M
import Control.Monad.State
import Data.Yaml.Aeson

import Sgf.Common
import Sgf.Control.Lens
import Sgf.System.Libvirt.Types
import Sgf.System.Libvirt.XML
import Sgf.System.Libvirt.Template


{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}



{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}

virshVolCreate :: (MonadError VmError m, MonadIO m) => Volume -> m ()
virshVolCreate _    = return ()

virshVolPath :: (MonadError VmError m, MonadIO m) => Volume -> m F.FilePath
virshVolPath v      = return $
                        "/dev" </> F.fromText (showt (fromLast $ volPool v))
                               </> F.fromText (showt (volName v))

type P a            = StateT PState (ReaderT Config (ExceptT VmError IO)) a

type IPMap          = M.Map IP [Domain]

data PState         = PState {domain :: Domain, ipMap :: IPMap}
  deriving (Show)
defPState :: PState
defPState           = PState {domain = mempty, ipMap = mempty}

runP :: P () -> IO ()
runP mx        = do
    r <- runExceptT . flip runReaderT defConfig . flip runStateT defPState $ mx
    liftIO $ case r of
      Left (XmlGenError pe) -> printParserError loadFile pe >>= putStrLn
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
newtype PlanConf    = PlanConf {planDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)
newtype OsConf      = OsConf {osDomain :: Domain}
  deriving (Show, FromJSON, ToJSON)

mergeConfigs :: Name -> SystemConf -> Domain -> PState
mergeConfigs dn SystemConf{..} d0 =
    let d = sysDomain <> d0
        d' = d{ name = dn
              , volume = (sysVolume <> volume d){volName = addVolNames (volume d)}
              }
    in  PState{domain = d', ipMap = sysIpMap}
  where
    addVolNames :: Volume -> Name
    addVolNames v   = dn +++ volName sysVolume +++ volName v
    (+++) :: Name -> Name -> Name
    x +++ y         = maybeSep "-" x y

-- | Append 'Monoid' values adding separator between them, if /both/ are not
-- 'mempty'.
maybeSep :: (Eq a, Monoid a) => a -> a -> a -> a
maybeSep s x y
  | x == mempty     = y
  | y == mempty     = x
  | otherwise       = x <> s <> y

setVolPath :: (MonadError VmError m, MonadIO m) => Domain -> m Domain
setVolPath d        = do
    let v = volume d
    virshVolCreate v
    p <- virshVolPath v
    return d{volume = v{volPath = pure (Path p)}}

work :: P ()
work                = do
    Config{..} <- ask

    -- Set default initial 'Domain' and load available IPs.
    scf <- liftVmError $ decodeFileEitherF sysConf
    PlanConf{..} <- liftVmError $ decodeFileEitherF planConf
    OsConf{..} <- liftVmError $ decodeFileEitherF osConf
    let ps0 = mergeConfigs domName scf (planDomain <> osDomain)
    put ps0
    PState{domain = dom0, ipMap = ipMap} <- get
    liftIO $ encodeFile "gen.yaml" dom0

    PState{domain = d1} <- get
    d2 <- setVolPath d1
    liftIO $ print d2
    modify (\ps -> ps{domain = d2})

    d <- gets domain
    ps1@PState{domain = d3, ipMap = ipMap1} <- get
    liftIO $ encodeFile "sys-gen.yaml" scf{sysIpMap = ipMap1}
    liftIO $ encodeFile "dom-gen.yaml" d3

    dh <- liftVmError $ genDomainXml d3 domTmpl
    liftIO $ T.writeFile "dom-gen.xml" dh
    let v = volume d3
    vh <- liftVmError $ genVolumeXml v volTmpl
    liftIO $ T.writeFile "vol-gen.xml" vh

-- FIXME: Do not assign number, if there is only one 'mempty' volume. Or just
-- do not assign number to first volume without name.
noEmptyName :: MonadState Int m => T.Text -> Volume -> m Volume
noEmptyName n v
  | volName v == mempty    = do
    i <- get
    put (i + 1)
    return $ if i == 1
      then v{volName = toName n}
      else v{volName = toName (n <> S.fromString (show i))}
  | otherwise               = return v
  where
    toName :: T.Text -> Name
    toName          = either (const mempty) id . parseName

data Config         = Config
                        { planConf  :: F.FilePath
                        , sysConf   :: F.FilePath
                        , osConf    :: F.FilePath
                        , domTmpl   :: F.FilePath
                        , volTmpl   :: F.FilePath
                        , domName   :: Name
                        , domIp     :: Maybe IP
                        }
  deriving (Show)

defConfig :: Config
defConfig           = Config
                        { planConf  = "../plan.yaml"
                        , sysConf   = "../system.yaml"
                        , osConf    = "../os.yaml"
                        , domTmpl   = "../dom.xml"
                        , volTmpl   = "../vol.xml"
                        , domName   = "test"
                        , domIp     = mempty
                        }

-- T.readFile "../volume.xml" >>= return . (\x -> gmapT (id `extT` volNameT x `extT` volSizeT x) defVolume) . parseXML
main :: IO ()
main                = do
    d3 <- F.readTextFile "../test4.xml" >>= initDomain (\p -> F.readTextFile $
            "../" </> F.decodeString (F.encodeString (basename p) <> "-vol") <.> "xml")
    liftIO $ encodeFile "test4-read.yaml" d3
    {-cv <- T.readFile "../vol.xml"
    let vol2 = readVolumeXml cv
    print vol2-}

