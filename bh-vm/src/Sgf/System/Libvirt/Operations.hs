{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}

module Sgf.System.Libvirt.Operations
  where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS ((</>), (<.>), basename)
import qualified Filesystem.Path.CurrentOS as F
import TextShow
import Control.Monad.Except

import Sgf.Common
import Sgf.System.Libvirt.Types

-- | @virsh list --all@
virshListAll :: MonadIO m => m [Name]
virshListAll        = return ["test4", "test5"]

-- | @virsh dumpxml@
virshDumpXml :: MonadIO m => Name -> m T.Text
virshDumpXml n      = liftIO . T.readFile . F.encodeString $
                        "./test/configs" </> F.fromText (showt n) <.> "xml"

-- | @virsh vol-dumpxml@
virshVolDumpXml :: MonadIO m => F.FilePath -> m T.Text
virshVolDumpXml f   = liftIO $ T.readFile ("./test/configs/" <> F.encodeString (basename f) <> "-vol.xml")

-- | @virsh vol-create@
virshVolCreate :: (MonadError VmError m, MonadIO m) => Volume -> m ()
virshVolCreate _    = return ()

-- | @virsh vol-path@
virshVolPath :: (MonadError VmError m, MonadIO m) => Volume -> m F.FilePath
virshVolPath v      = return $
                        "/dev" </> F.fromText (showt (fromLast $ volPool v))
                               </> F.fromText (showt (volName v))

