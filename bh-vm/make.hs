#!/usr/bin/env stack
-- stack runghc --install-ghc --package shake

import System.Directory
import System.Environment
import Development.Shake


main :: IO ()
main                = do
    argv <- getArgs
    createDirectoryIfMissing True "_shake"
    unit $ cmd "stack ghc"
        [ "--", "-iapp", "--make", "app/Build.hs"
        , "-rtsopts", "-with-rtsopts=-I0"
        , "-outputdir=_shake", "-o", "_shake/build"
        ]
    unit $ cmd "_shake/build" argv
