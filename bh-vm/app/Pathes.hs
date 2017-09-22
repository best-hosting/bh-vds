{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module: Pathes
--
-- Functions returning pathes to different directories used by `bh-vm` binary
-- and `build` depending on installation prefix and corresponding file
-- patterns.
--
-- Note, that i shouldn't use `FilePath` from `system-filepath` package here,
-- because this module dependencies affect `build` binary dependencies and are
-- effectively initial requirments to start building. So i'd better keep
-- dependencies at minimum.
module Pathes
  where

import Development.Shake (FilePattern)
import System.FilePath


-- | Given a prefix return a path to preset config directory and pattern to
-- match preset config files.
planConfFilePat :: FilePath -> (FilePath, [FilePattern])
planConfFilePat p   = (p </> "plan", ["*.yaml"])

-- | Given a prefix return a path to os config directory and pattern to match
-- os config files.
osConfFilePat :: FilePath -> (FilePath, [FilePattern])
osConfFilePat p     = (p </> "os", ["*.yaml"])

-- | Given a prefix return a path to system config directory and pattern to
-- match system config files.
sysConfFilePat :: FilePath -> (FilePath, [FilePattern])
sysConfFilePat p    = (p, ["*.yaml"])

-- | Given a prefix return a path to template directory and pattern to match
-- template files.
tmplConfFilePat :: FilePath -> (FilePath, [FilePattern])
tmplConfFilePat p   = (p </> "template", ["*.xml"])

