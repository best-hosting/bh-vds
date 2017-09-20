
-- |
-- Module: System.Libvirt.IP
--
-- Functions working with domain 'IP'-s.

module System.Libvirt.IP
    (
      buildDomSet
    , buildIPMap
    , findIP
    )
  where

import           Control.Monad.Reader
import           Control.Exception
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Filesystem.Path.CurrentOS  as F

import           Internal.Common
import           System.Libvirt.Types
import           System.Libvirt.XML
import           System.Libvirt.Operations

-- | Read all domains from @virsh@ and build a 'S.Set'.
buildDomSet :: MonadIO m => m (S.Set Domain)
buildDomSet         = virshListAll >>= foldM go S.empty
  where
    --go :: S.Set Domain -> Name -> m (S.Set Domain)
    go zs n         = do
        c <- virshDumpXml n
        d <- initDomain virshVolDumpXml c
        return (S.insert d zs)

-- | Add domains from a 'S.Set' to an 'IPMap'.
buildIPMap :: IPMap -> S.Set Domain -> IPMap
buildIPMap z0       = IPMap . S.fold (\d -> M.insertWith S.union
                                                (fromLast (ip d))
                                                (S.singleton d))
                             (getIPMap z0)

-- | Find an unused 'IP' or check that requested 'IP' is available. Note,
-- 'findIP' does /not/ update information about returned 'IP' in 'IPMap'.
-- Because domain definition may not be complete yet.
findIP :: MonadIO m => F.FilePath -> Maybe IP -> IPMap -> m IP
findIP sysConfFile domIp ipm
  | not (null freeIPs)  =
        case domIp of
          Just x
            | M.member x usedIPs
                        -> throw (IPAlreadyInUse x (usedBy x))
            | x `notElem` freeIPs
                        -> throw (IPNotAvailable x sysConfFile)
            | otherwise -> return x
          Nothing       -> return (head freeIPs)
  | otherwise           =  throw (NoFreeIPs sysConfFile)
  where
    usedIPs :: M.Map IP (S.Set Domain)
    usedIPs         = M.filter (not . S.null) (getIPMap ipm)
    freeIPs :: [IP]
    freeIPs         = M.keys (M.filter S.null (getIPMap ipm))
    usedBy :: IP -> [Domain]
    usedBy          = maybe [] S.toList . flip M.lookup usedIPs

