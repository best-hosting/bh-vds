{-# LANGUAGE OverloadedStrings  #-}

module Pathes
  where

import Development.Shake (FilePattern)
import qualified Filesystem.Path.CurrentOS as F


-- | Given a prefix return a path to preset config directory and pattern to
-- match preset config files.
planConfFilePat :: F.FilePath -> (F.FilePath, [FilePattern])
planConfFilePat p   = (p F.</> "preset", ["*.yaml"])

-- | Given a prefix return a path to os config directory and pattern to match
-- os config files.
osConfFilePat :: F.FilePath -> (F.FilePath, [FilePattern])
osConfFilePat p     = (p F.</> "os", ["*.yaml"])

-- | Given a prefix return a path to system config directory and pattern to
-- match system config files.
sysConfFilePat :: F.FilePath -> (F.FilePath, [FilePattern])
sysConfFilePat p    = (p, ["*.yaml"])

-- | Given a prefix return a path to template directory and pattern to match
-- template files.
tmplConfFilePat :: F.FilePath -> (F.FilePath, [FilePattern])
tmplConfFilePat p   = (p F.</> "template", ["*.xml"])

