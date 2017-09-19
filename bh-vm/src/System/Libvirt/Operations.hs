{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE CPP                #-}

-- |
-- Module: System.Libvirt.Operations
--
-- Basic @libvirt@ operations (implemented as calls to @virsh@).

module System.Libvirt.Operations
    (
    -- * Low-level libvirt operations.
    --
    -- $lowlevel
      virshListAll
    , virshDumpXml
    , virshVolDumpXml
    , virshVolCreate
    , virshVolDelete
    , virshVolPath
    , virshDefine
    , virshUndefine
    )
  where

import           Data.Text (pack, dropWhileEnd)
import qualified Filesystem.Path.CurrentOS  as F
import           TextShow (showt)
import           Control.Monad.Except
import           Turtle
import           Control.Foldl (list)
import           Control.Exception
import           Control.Monad.Managed
import           System.Directory

import           System.Libvirt.Types

import           Internal.Common


-- $virsh

-- | Call to @virsh@ do not expecting any output.
virsh :: MonadIO m => [Text] -> Shell Line -> m ()
#ifndef TEST
virsh argv inp      = procs "virsh" argv inp
#else
virsh ("vol-create" : _) _  = return ()
virsh ("define"     : _) _  = return ()
virsh argv               _  = error $
    "virsh: Unsupported virsh options for test mode: " ++ show argv
#endif

-- | Call to @virsh@ expecting some output.
invirsh :: [Text] -> Shell Line -> Shell Line
#ifndef TEST
invirsh argv inp    = inproc "virsh" argv inp
#else
invirsh ("list" : _) _
                    = select . textToLines $ "test4\ntest5"
invirsh ["dumpxml", n] _
                    = input $ "./test/configs" </> fromText n <.> "xml"
invirsh ["vol-dumpxml", p] _
                    = let n = F.encodeString (F.basename (fromText p))
                      in  input $ "./test/configs"
                                </> F.decodeString (n <> "-vol.xml")
invirsh ["vol-path", "--pool", p, n] _
                    = select . textToLines . pack . F.encodeString $
                        "/dev"  </> fromText p </> fromText n
invirsh argv _      = error $
    "invirsh: Unsupported virsh options for test mode: " ++ show argv
#endif

-- $lowlevel

-- | @virsh list --all@
virshListAll :: MonadIO m => m [Name]
virshListAll        = flip fold list $ do
    ns <- grep chars1 $ invirsh ["list", "--all", "--name"] empty
    either (const empty) return . parseName $ lineToText ns

-- | @virsh dumpxml@
virshDumpXml :: MonadIO m => Name -> m Text
virshDumpXml n      = strict $ invirsh  ["dumpxml", showt n] empty

-- | @virsh vol-dumpxml@
--
-- Note: @virsh vol-dumpxml@ may work on /full path/ to volume /without/ pool
-- name specified.
virshVolDumpXml :: MonadIO m => F.FilePath -> m Text
virshVolDumpXml p   = strict $ invirsh  [ "vol-dumpxml"
                                        , pack (F.encodeString p)
                                        ]
                                        empty

-- | @virsh vol-create@
virshVolCreate :: MonadIO m => Volume -> F.FilePath -> m Volume
virshVolCreate v@Volume{..} xf  = do
    sh $ virsh
            [ "vol-create"
            , "--pool", showt (fromLast volPool)
            , pack (F.encodeString xf)
            ]
            empty
    return v

-- | @virsh vol-delete@
virshVolDelete :: MonadIO m => Volume -> m ()
virshVolDelete Volume{..}   =
    sh $ procs "virsh"
            [ "vol-delete"
            , "--pool", showt (fromLast volPool)
            , showt volName
            ]
            empty

-- | @virsh vol-path@
--
-- Note: all trailing newlines are dropped from @virsh vol-path@ output, so,
-- generally, newlines in filenames are /not/ supported.
virshVolPath :: MonadIO m => Volume -> m F.FilePath
virshVolPath Volume{..} = fmap (fromText . dropWhileEnd (== '\n')) . strict $
    invirsh
        [ "vol-path"
        , "--pool", showt (fromLast volPool), showt volName
        ]
        empty

-- | @virsh define@
virshDefine :: MonadIO m => Domain -> F.FilePath -> m Domain
virshDefine d xf    = do
    sh $ virsh ["define", pack (F.encodeString xf)] empty
    return d

-- | @virsh undefine@
virshUndefine :: MonadIO m => Domain -> m ()
virshUndefine Domain{..} = sh $ virsh ["undefine", showt name] empty

