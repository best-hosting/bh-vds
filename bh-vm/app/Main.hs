{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Monoid
import qualified Data.Text as T
import Options.Applicative
import Development.Shake (FilePattern, (?==))
import System.FilePath
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad

import Pathes
import System.Libvirt.Types
import System.Libvirt.Config
import System.Libvirt

#if LOCAL
prefix :: FilePath
prefix              = ".."
#else
import Build.Pathes (prefix)
#endif


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


-- | Use matcher function to match against supplied 'String' and, if match
-- suceeded, return full path to config file with that name. Note, that file
-- existence is _not_ checked.
matchConf :: (FilePath -> (FilePath, [FilePattern]))
             -> String -> Either String F.FilePath
matchConf f t       = let (d, ps) = f prefix in
    case foldr go ([], []) ps of
        ([r], _)    -> Right (F.decodeString (d </> r))
        ([] , [w])  -> Right (F.decodeString (d </> w))
        ([] , [])   -> Left $ "Not a valid plan name: " ++ t
        (rs , ws)   -> Left $ "More, than one pattern matches: "
                                ++ show rs ++ ", " ++ show ws
  where
    -- | Try to match string against entire 'FilePattern' or only against its
    -- basename.
    go :: FilePattern
          -> ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
    go p (zs, ws)   =
        ( if p ?== t then t : zs else zs
        , if takeBaseName p ?== t
                     then (t <.> takeExtension p) : ws else ws
        )

-- | Command-line options.
options :: Parser Config
options             = Config
    <$> option (eitherReader (matchConf planConfFilePat))
            (   long "plan"
            <>  short 'p'
            <>  metavar "PLAN"
            <>  help "Name of plan to use."
            )
    <*> either error pure (matchConf sysConfFilePat "system")
    <*> option (eitherReader (matchConf osConfFilePat))
            (   long "os"
            <>  short 'o'
            <>  metavar "OS"
            <>  help "Name of OS to use."
            )
    <*> either error pure (matchConf tmplConfFilePat "dom")
    <*> either error pure (matchConf tmplConfFilePat "vol")
    <*> option (eitherReader (parseName . T.pack))
            (   long "name"
            <>  short 'n'
            <>  metavar "NAME"
            <>  help "New vm name."
            )
    <*> (option (eitherReader (fmap Just . parseIP . T.pack))
            (   long "ip"
            <>  short 'i'
            <>  metavar "IP"
            <>  help "IP address for new vm."
            )
        <|> pure Nothing)

main :: IO ()
main                = join . execParser $
    info (helper <*> (runP <$> options <*> pure addVm))
    (  fullDesc
    <> header "General program title/description"
    <> progDesc "What does this thing do?"
    )

