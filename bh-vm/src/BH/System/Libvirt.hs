{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}

-- |
-- Module: BH.System.Libvirt
--
-- Main operations provided by @bh-vm@.

module BH.System.Libvirt
    (
    -- * Operations.
    --
    -- $operations
      readSystemConf
    , writeSystemConf
    , mergeConfigs
    , loadConfigs
    , findIP
    , acquireIP
    , createVolume

    -- * Main.
    --
    -- $main
    , addVm

    -- * Libvirt operations in 'MonadManaged'.
    --
    -- $managed
    , createVolume
    , createDomain
    )
  where

import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.String            as S
import           Control.Monad.Except
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Control.Monad.State
import           Control.Exception
import           Control.Monad.Managed
import           Data.Yaml.Aeson
import qualified Filesystem.Path.CurrentOS  as F
import           System.Directory

import           System.Libvirt.Types
import           System.Libvirt.XML
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

-- | Load all configs.
-- Note: at the end i /rescan/ 'IPMap' and overwrite 'sysConfFile'. I.e. i
-- don't use internal 'IPMap' state and just ask @libvirt@.
loadConfigs :: P ()
loadConfigs         = do
    Config{..}  <- ask
    scf <- using $ managed $
            bracket (readSystemConf sysConfFile)
                    (writeSystemConf sysConfFile <=< buildIPMap')
    PlanConf{..} <- decodeFileEither' planConfFile
    OsConf{..}   <- decodeFileEither' osConfFile
    put (mergeConfigs domName scf (planDomain <> osDomain))


-- | Note, 'findIP' does /not/ update information about returned 'IP' in
-- 'IPMap'. Because.. domain definition may not be complete yet.
findIP :: MonadIO m => Config -> IPMap -> m IP
findIP Config{..} ipm
  | not (null freeIPs)  =
        case domIp of
          Just ip
            | M.member ip usedIPs
                        -> throw (IPAlreadyInUse ip (usedBy ip))
            | ip `notElem` freeIPs
                        -> throw (IPNotAvailable ip sysConfFile)
            | otherwise -> return ip
          Nothing       -> return (head freeIPs)
  | otherwise           =  throw (NoFreeIPs sysConfFile)
  where
    usedIPs :: M.Map IP (S.Set Domain)
    usedIPs         = M.filter (not . S.null) (getIPMap ipm)
    freeIPs :: [IP]
    freeIPs         = M.keys (M.filter S.null (getIPMap ipm))
    usedBy :: IP -> [Domain]
    usedBy          = maybe [] S.toList . flip M.lookup usedIPs

acquireIP :: P ()
acquireIP           = do
    cf@Config{..}   <- ask
    PState{..}      <- get
    i <- findIP cf ipMap
    modify (\ps -> ps{domain = domain{ip = toLast i}})

-- $managed

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

