{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Monoid
import qualified Data.Text as T
import qualified Options.Applicative as A
import Filesystem.Path.CurrentOS ((</>), (<.>), basename)
import qualified Filesystem.Path.CurrentOS as F
import Control.Applicative
import Control.Monad

import Lib
import Sgf.System.Libvirt.Types


-- Read system values and cmd.
--
--  cmd     = name + ip + tariff + template + arch (?)
--  tariff  = memory + size
--  system  = pool + bridge
--
--  Overwrite: cmd > tariff > system
--
--  map template --> cd iso path
--
-- virsh vol-create besthost133 gen-vol.xml
-- virsh vol-path vm-abc --pool besthost133  ==> volpath
-- virsh define gen-dom.xml
-- attach install cd.
--
-- Checks:
--
--  - volpath not used by any other vm. Use readlink. Path exists - ask.
--  - ip not used by any other vm. Warn about vms without explicit ip spec.
--  Fix plain text db: virsh --> db.
--
-- Assume: default net (instead of specifying bridge directly) and default
-- pool (name it default). Then i may omit system config altogether.


configPath :: F.FilePath
configPath          = "../"

presetConfPath, osConfPath, sysConfPath, tmplPath :: F.FilePath -> F.FilePath
presetConfPath  x   = configPath </> "preset"   </> x <.> "yaml"
osConfPath      x   = configPath </> "os"       </> x <.> "yaml"
sysConfPath     x   = configPath                </> x
tmplPath        x   = configPath </> "template" </> x <.> "xml"

-- | Command-line options.
options :: A.Parser Config
options             = Config
    <$> A.option (A.eitherReader (pure . presetConfPath . F.decodeString))
            (   A.long "preset"
            <>  A.short 'p'
            <>  A.metavar "PRESET"
            <>  A.help "Name of preset to use."
            )
    <*> pure (sysConfPath "system.yaml")
    <*> A.option (A.eitherReader (pure . osConfPath . F.decodeString))
            (   A.long "os"
            <>  A.short 'o'
            <>  A.metavar "OS"
            <>  A.help "Name of OS to use."
            )
    <*> pure (tmplPath "dom")
    <*> pure (tmplPath "vol")
    <*>  A.option (A.eitherReader (parseName . T.pack))
            (   A.long "name"
            <>  A.short 'n'
            <>  A.metavar "NAME"
            <>  A.help "New vm name."
            )
    <*> (A.option (A.eitherReader (fmap Just . parseIP . T.pack))
            (   A.long "ip"
            <>  A.short 'i'
            <>  A.metavar "IP"
            <>  A.help "IP address for new vm."
            )
        <|> pure Nothing)

main :: IO ()
main                = join . A.execParser $
    A.info (A.helper <*> (runP <$> options <*> pure work))
    (  A.fullDesc
    <> A.header "General program title/description"
    <> A.progDesc "What does this thing do?"
    )

