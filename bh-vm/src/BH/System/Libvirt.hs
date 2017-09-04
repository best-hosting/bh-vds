{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards    #-}

module BH.System.Libvirt
    ( buildDomSet
    , buildIPMap
    , setVolPath
    , mergeConfigs
    , defineVm
    )
  where

import Data.Maybe
import Prelude      hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import Data.Monoid
import qualified Data.String as S
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Yaml.Aeson

import System.Libvirt.Types
import System.Libvirt.XML
import System.Libvirt.Template
import System.Libvirt.Operations
import BH.System.Libvirt.Types

import Internal.Common


-- | Read all domains from @virsh@.
buildDomSet :: MonadIO m => m (S.Set Domain)
buildDomSet         = virshListAll >>= foldM go S.empty
  where
    --go :: S.Set Domain -> Name -> m (S.Set Domain)
    go zs n         = do
        c <- virshDumpXml n
        d <- initDomain virshVolDumpXml c
        return (S.insert d zs)

-- | Add domains from a 'Set' to an 'IPMap'.
buildIPMap :: IPMap -> S.Set Domain -> IPMap
buildIPMap z0       = S.fold (\d -> M.insertWith S.union
                                                 (fromLast (ip d))
                                                 (S.singleton d))
                             z0


-- | Merge 'SystemConf' into initial 'Domain' value.
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
    in  PState{domain = d', ipMap = M.map (const S.empty) sysIpMap}
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

defineVm :: P ()
defineVm            = do
    Config{..} <- ask

    -- Set default initial 'Domain' and load available IPs.
    scf          <- liftVmError $ decodeFileEitherF sysConfFile
    PlanConf{..} <- liftVmError $ decodeFileEitherF planConfFile
    OsConf{..}   <- liftVmError $ decodeFileEitherF osConfFile
    let ps0@PState{domain = dom0, ipMap = ipMap0}
                    = mergeConfigs domName scf (planDomain <> osDomain)
    put ps0
    liftIO $ encodeFile "dom0.yaml" dom0
    liftIO $ encodeFile "ipmap0.yaml" scf{sysIpMap = ipMap0}

    dset <- buildDomSet
    let ipm = buildIPMap ipMap0 dset
        usedIPs = M.filter (not . S.null) ipm
        freeIPs = M.filter S.null ipm
    when (M.null freeIPs) $ error "No free IPs."

    dip <- case domIp of
      Just ip
        | M.member ip usedIPs    -> error $ "IP already used by "
                                        ++ show (M.lookup ip usedIPs)
        | M.notMember ip freeIPs -> error $ "IP is not available. "
                                        ++ "Add it to 'system.yaml' first."
        | otherwise -> return ip
      Nothing   -> return $ head (M.keys freeIPs)

    d <- setVolPath dom0{ip = toLast dip}
    modify (\ps -> ps{ domain = d
                     , ipMap = buildIPMap ipMap0 (S.insert d dset)
                     })

    gets ipMap  >>= \im ->
                    liftIO $ encodeFile "sys-gen.yaml" scf{sysIpMap = im}
    gets domain >>= liftIO . encodeFile "dom-gen.yaml"

    dh <- liftVmError $ genDomainXml d domTmplFile
    liftIO $ T.writeFile "dom-gen.xml" dh
    let v = volume d
    vh <- liftVmError $ genVolumeXml v volTmplFile
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

