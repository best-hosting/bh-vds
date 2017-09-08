{-# LANGUAGE GeneralizedNewtypeDeriving     #-}

module System.Libvirt.IP
    (
      IPMap (..)
    , buildDomSet
    , buildIPMap
    )
  where

import qualified Data.Text                  as T
import           TextShow
import           Control.Monad.Reader
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Yaml.Aeson

import Internal.Common
import System.Libvirt.Types
import System.Libvirt.XML
import System.Libvirt.Operations

-- | Map from 'IP' to set of 'Domain'-s using it.
newtype IPMap       = IPMap {getIPMap :: M.Map IP (S.Set Domain)}
  deriving (Show, Monoid)

-- | Convert 'Text' map to 'IP' map. Needed for reading 'IPMap' from json.
instance FromJSON IPMap where
    parseJSON       = fmap parseIPMap . parseJSON
      where
        parseIPMap :: M.Map T.Text (S.Set Domain) -> IPMap
        parseIPMap  = IPMap . M.foldrWithKey go M.empty
          where
            --go :: T.Text -> a -> M.Map IP a -> M.Map IP a
            go x d zm   = either (const zm) (\y -> M.insert y d zm)
                            . parseIP $ x

instance ToJSON IPMap where
    toJSON          = toJSON . M.mapKeys showt . getIPMap

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

