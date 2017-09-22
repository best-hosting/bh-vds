{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}

-- |
-- Module: System.Libvirt
--
-- Main operations provided by @bh-vm@.

module System.Libvirt
    (
    -- * Basic operations.
    --
    -- $operations
      readSystemConf
    , writeSystemConf
    , acquireIP

    -- * 'managed' libvirt operations.
    --
    -- $managed
    , loadConfigs
    , createVolume
    , createDomain

    -- * Main.
    --
    -- $main
    , addVm
    )
  where

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.String            as S
import qualified Data.Map               as M
import           Control.Monad.State
import           Control.Exception
import           Control.Monad.Managed
import           Data.Yaml.Aeson
import qualified Filesystem.Path.CurrentOS  as F
import           System.Directory

import           System.Libvirt.Types
import           System.Libvirt.Template
import           System.Libvirt.IP
import           System.Libvirt.Operations
import           System.Libvirt.Config

import           Internal.Common


-- $operations


-- $main

-- | Rebuild 'IPMap' in 'SystemConf'.
buildIPMap' :: MonadIO m => SystemConf -> m SystemConf
buildIPMap' scf@SystemConf{..} = do
    ds <- buildDomSet
    return (scf{sysIpMap = buildIPMap (clearIPMap sysIpMap) ds})
  where
    -- | Clear all data from an 'IPMap', so only 'IP'-s remain.
    clearIPMap :: IPMap -> IPMap
    clearIPMap      = IPMap . M.map (const mempty) . getIPMap

-- | Read 'SystemConf' from system config file.
readSystemConf :: MonadIO m => F.FilePath -> m SystemConf
readSystemConf scfFile  = decodeFileEither' scfFile >>= buildIPMap'

-- | Write 'SystemConf' to system config file.
writeSystemConf :: MonadIO m => F.FilePath -> SystemConf -> m ()
writeSystemConf scfFile scf = liftIO $ do
    let old = F.encodeString scfFile
        new = F.encodeString (scfFile F.<.> ".new")
    encodeFile new scf
    renameFile new old

-- | Acquire 'IP' for a 'domain'.
acquireIP :: P ()
acquireIP           = do
    Config{..}  <- ask
    PState{..}  <- get
    i <- findIP sysConfFile domIp ipMap
    modify (\ps -> ps{domain = domain{ip = toLast i}})


-- $managed

-- | Load all configs and at the end /rescan/ 'IPMap' and overwrite
-- 'sysConfFile'. I.e. i don't use internal 'IPMap' state and just ask
-- @libvirt@.
loadConfigs :: P ()
loadConfigs         = do
    Config{..}  <- ask
    scf <- using $ managed $
            bracket (readSystemConf sysConfFile)
                    (writeSystemConf sysConfFile <=< buildIPMap')
    PlanConf{..} <- decodeFileEither' planConfFile
    OsConf{..}   <- decodeFileEither' osConfFile
    put (mergeConfigs domName scf (planDomain <> osDomain))

-- | Create libvirt volume in a safe way: if later computation fails, created
-- volume will be deleted.
createVolume :: P ()
createVolume        = do
    Config{..} <- ask
    PState{..} <- get
    let v = volume domain
    xml <- genVolumeXml v volTmplFile
    liftIO $ T.writeFile "vol-gen.xml" xml
    tf <- writeTempFile "createVolume" xml
    v' <- using $ managed $
            bracketOnError (virshVolCreate v tf) virshVolDelete
    p  <- virshVolPath v'
    modify (\ps -> ps{domain = domain{volume = v'{volPath = pure (Path p)}}})

-- | Create libvirt domain in a safe way: if later computation fails, created
-- domain will be deleted.
createDomain :: P ()
createDomain        = do
    Config{..}  <- ask
    PState{..}  <- get
    xml <- genDomainXml domain domTmplFile
    liftIO $ T.writeFile "dom-gen.xml" xml
    tf <- writeTempFile "createDomain" xml
    d  <- using $ managed $
            bracketOnError (virshDefine domain tf) virshUndefine
    virshAutostart d
    modify (\ps -> ps{domain = d})

-- | Read configs and define a 'Domain' in libvirt.
addVm :: P ()
addVm               = do
    loadConfigs
    acquireIP
    createVolume
    createDomain


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

