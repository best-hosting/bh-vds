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
      buildDomSet
    , buildIPMap
    , setVolPath
    , mergeConfigs

    -- * Main.
    --
    -- $main
    , defineVm
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

import           System.Libvirt.Types
import           System.Libvirt.XML
import           System.Libvirt.Template
import           System.Libvirt.IP
import           System.Libvirt.Operations
import           System.Libvirt.Config

import           Internal.Common


-- $operations

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
    in  PState{domain = d', ipMap = IPMap . M.map (const S.empty) . getIPMap $ sysIpMap}
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

-- | Create 'Domain' 's 'Volume' in libvirt and set its path.
setVolPath :: (MonadError VmError m, MonadIO m) => Domain -> m Domain
setVolPath d@Domain{..} = do
    p <- virshVolPath volume
    return d{volume = volume{volPath = pure (Path p)}}


-- $main

freeIPs :: IPMap -> S.Set IP
freeIPs             = S.fromList . M.keys . M.filter S.null . getIPMap

findIP :: MonadIO m => Config -> IPMap -> Maybe IP -> m IP
findIP Config{..} ipm mip
  | not (null freeIPs')  =
        let usedIPs = M.filter (not . S.null) (getIPMap ipm)
        in  case mip of
                Just ip
                  | M.member ip usedIPs    -> throw $
                      IPAlreadyInUse ip (maybe [] S.toList (M.lookup ip usedIPs))
                  | S.notMember ip freeIPs' ->
                      throw $
                        IPNotAvailable ip sysConfFile
                  | otherwise -> return ip
                Nothing   -> return (head (S.toList freeIPs'))
  | otherwise       =
      throw $
            NoFreeIPs sysConfFile
  where
    freeIPs' :: S.Set IP
    freeIPs'    = freeIPs ipm

acquireIP :: MonadIO m => Config -> IPMap -> Domain -> m Domain
acquireIP cf@Config{..} ipm0 d     = do
    dset <- buildDomSet
    i <- findIP cf (buildIPMap ipm0 dset) domIp
    return d{ip = toLast i}

readIPM :: MonadIO m => SystemConf -> m IPMap
readIPM SystemConf{..}  = buildDomSet >>= return . buildIPMap sysIpMap

readSystemConf :: MonadIO m => F.FilePath -> m SystemConf
readSystemConf sf       = undefined

writeIPM :: MonadIO m => F.FilePath -> SystemConf -> m ()
writeIPM scfFile scf = do
    ipm <- readIPM scf
    liftIO $ encodeFile (F.encodeString scfFile <> ".new") scf{sysIpMap = ipm}

acquireIP2 :: MonadManaged m => Config -> SystemConf -> Domain -> m Domain
acquireIP2 cf@Config{..} scf d    = do
    ipm <- using $ managed $
        bracket (readIPM scf) (\_ -> writeIPM sysConfFile scf)
    i <- findIP cf ipm domIp
    return d{ip = toLast i}

-- | Read configs and define a 'Domain' in libvirt.
defineVm :: P ()
defineVm            = do
    cf@Config{..} <- ask

    -- Set default initial 'Domain' and load available IPs.
    --scf          <- liftVmError $ decodeFileEitherF sysConfFile
    scf          <- decodeFileEither' sysConfFile
    PlanConf{..} <- decodeFileEither' planConfFile
    OsConf{..}   <- decodeFileEither' osConfFile
    let ps0@PState{domain = dom0, ipMap = ipMap0}
                    = mergeConfigs domName scf (planDomain <> osDomain)
    put ps0
    liftIO $ encodeFile "dom0.yaml" dom0
    liftIO $ encodeFile "ipmap0.yaml" scf{sysIpMap = ipMap0}

    d' <- acquireIP2 cf scf dom0

    let v = volume d'
    vh <- genVolumeXml v volTmplFile
    liftIO $ T.writeFile "vol-gen.xml" vh
    v' <- createVolume v vh
    let d = d'{volume = v'}

    {-modify (\ps -> ps{ domain = d
                     , ipMap = buildIPMap ipMap0 (S.insert d dset)
                     })

    gets ipMap  >>= \im ->
                    liftIO $ encodeFile "sys-gen.yaml" scf{sysIpMap = im}-}
    gets domain >>= liftIO . encodeFile "dom-gen.yaml"

    dh <- genDomainXml d domTmplFile
    liftIO $ T.writeFile "dom-gen.xml" dh
    virshDefine d dh

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

