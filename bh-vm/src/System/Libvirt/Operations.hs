{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE CPP                #-}

-- |
-- Module: System.Libvirt.Operations
--
-- Different basic `libvirt` operations (implemented as calls to `virsh`).

module System.Libvirt.Operations
  where

import qualified Filesystem.Path.CurrentOS  as F
import           TextShow (showt)
import           Control.Monad.Except
import           Turtle
import           Control.Foldl (list)
import           Control.Monad.Managed
#ifndef TEST
import           Data.Text (pack)
import           System.Directory
#endif

import           System.Libvirt.Types

import           Internal.Common


-- | @virsh list --all@
virshListAll :: MonadIO m => m [Name]
virshListAll        =
#ifndef TEST
    flip fold list $ do
        ns <- grep chars1 $ inproc "virsh" ["list", "--all", "--name"] empty
        either (const empty) return . parseName $ lineToText ns
#else
    return ["test4", "test5"]
#endif

-- | @virsh dumpxml@
virshDumpXml :: MonadIO m => Name -> m Text
virshDumpXml n      =
#ifndef TEST
    strict $ inproc "virsh" ["dumpxml", showt n] empty
#else
    liftIO . readTextFile $
        "./test/configs" </> fromText (showt n) <.> "xml"
#endif

-- | @virsh vol-dumpxml@
-- Note, that @vol-dumpxml@ may work on /full path/ to volume /without/ pool
-- name specified.
virshVolDumpXml :: MonadIO m => F.FilePath -> m Text
virshVolDumpXml p   =
#ifndef TEST
    strict $ inproc "virsh" ["vol-dumpxml", pack (F.encodeString p)] empty
#else
    liftIO . readTextFile $ "./test/configs" </>
        F.decodeString (F.encodeString (F.basename p) <> "-vol.xml")
#endif

-- | @virsh vol-create@
virshVolCreate :: MonadManaged m =>
                    Volume -> Text -> m ()
virshVolCreate Volume{..} t = do
#ifndef TEST
    tmp <- liftIO getTemporaryDirectory
    tf  <- mktempfile (F.decodeString tmp) "virshVolCreate"
    liftIO $ writeTextFile tf t
    sh $ procs "virsh"
            [ "vol-create"
            , "--pool", showt (fromLast volPool)
            , pack (F.encodeString tf)
            ]
            empty
#else
    return ()
#endif

-- | @virsh vol-path@
virshVolPath :: MonadIO m => Volume -> m F.FilePath
virshVolPath Volume{..} =
#ifndef TEST
    fmap fromText . strict . grep chars1 $
        inproc "virsh"
            [ "vol-path"
            , "--pool", showt (fromLast volPool), showt volName
            ]
            empty
#else
    return $
        "/dev" </> fromText (showt (fromLast volPool))
               </> fromText (showt volName)
#endif

-- | @virsh define@
virshDefine :: MonadManaged m => Domain -> Text -> m ()
virshDefine _ t     = do
#ifndef TEST
    -- FIXME: Wrapper around `getTemporaryDirectory` and `mktempfile` and,
    -- probably, `writeTextFile` ?
    tmp <- liftIO getTemporaryDirectory
    tf  <- mktempfile (F.decodeString tmp) "virshVolCreate"
    liftIO $ writeTextFile tf t
    sh $ procs "virsh" ["define", pack (F.encodeString tf)] empty
#else
    return ()
#endif

