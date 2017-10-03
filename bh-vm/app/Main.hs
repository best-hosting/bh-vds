{-# LANGUAGE CPP                #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Monoid
import Data.Either
import qualified Data.Text as T
import Options.Applicative
import Development.Shake (FilePattern, (?==))
import System.FilePath
import qualified System.Directory          as IO
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad

import Pathes
import System.Libvirt.Types
import System.Libvirt.Config
import System.Libvirt

#if LOCAL
prefix :: FilePath
prefix              = ".."
version :: FilePath
version             = "Local build"
#else
import Build.Pathes (prefix)
import Build.Version (version)
#endif


-- | Use matcher function to match against supplied 'String' and, if match
-- suceeded, return full path to config file with that name. Note:
--
--  * File existence is _not_ checked.
--  * I will try to match 'String' both against _full_ 'FilePattern' and, if
--  'String' _does not_ have an extension, against only _basename_ of
--  'FilePattern'.
--  * I require single match, because if i have several file patterns and
--  'String' without extension, _both_ patterns may match during _basename_
--  attempt. And since i don't check file existence, i can't filter out wrong
--  result.
matchConf :: (FilePath -> (FilePath, [FilePattern]))
             -> String -> Either String F.FilePath
matchConf f t       = let (d, ps) = f prefix in
    case foldr go ([], []) ps of
        ([r], _)    -> Right (F.decodeString (d </> r))
        ([] , [w])  -> Right (F.decodeString (d </> w))
        ([] , [])   -> Left $ "No config file matching name: '" ++ t ++ "'"
        (rs , ws)   -> Left $ "More, than one pattern matches: "
                                ++ show rs ++ ", " ++ show ws
  where
    -- | Try to match string against entire 'FilePattern' or only against its
    -- basename. I should try basename match only, if string does not have an
    -- extension, because otherwise e.g. if pattern is @*.yaml@ and string is
    -- @abc.txt@, file pattern basename will be @*@ and match always succeed,
    -- and resulting config name will be @abc.txt.yaml@ .
    go :: FilePattern
          -> ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
    go p (zs, ws)   =
        ( if p ?== t then t : zs else zs
        , if null (takeExtension t) && takeBaseName p ?== t
                     then (t <.> takeExtension p) : ws else ws
        )

-- | Use matcher function to find all matching _existing_ config files under
-- 'prefix' directory.
getMatchedConfs :: (FilePath -> (FilePath, [FilePattern]))
                    -> IO [F.FilePath]
getMatchedConfs f   = do
    let (d, _) = f prefix
    ls <- IO.getDirectoryContents d >>= onlyFiles d
    return (rights . map (matchConf f) $ ls)
  where
    onlyFiles :: FilePath -> [FilePath] -> IO [FilePath]
    onlyFiles d = filterM (IO.doesFileExist . (d </>))
                    . filter (`notElem` [".", ".."])

-- | Options for @add@ command.
addOptions :: Parser Config
addOptions          = Config
    <$> (option (eitherReader (fmap Just . matchConf planConfFilePat))
            (   long "plan"
            <>  short 'p'
            <>  metavar "PLAN"
            <>  help "Name of plan to use."
            ) <|> pure Nothing)
    <*> either error pure (matchConf sysConfFilePat "system")
    <*> (option (eitherReader (fmap Just . matchConf osConfFilePat))
            (   long "os"
            <>  short 'o'
            <>  metavar "OS"
            <>  help "Name of OS to use."
            ) <|> pure Nothing)
    <*> either error pure (matchConf tmplConfFilePat "domain")
    <*> either error pure (matchConf tmplConfFilePat "volume")
    <*> option (eitherReader (parseName . T.pack))
            (   long "name"
            <>  short 'n'
            <>  metavar "NAME"
            <>  help "New domain name."
            )
    <*> (option (eitherReader (fmap Just . parseIP . T.pack))
            (   long "ip"
            <>  short 'i'
            <>  metavar "IP"
            <>  help "IP address for new domain."
            )
        <|> pure Nothing)

-- Check and update config files. Try to build and load all possible
-- 'Config'-s from all config files i've found. And, as a consequence, write
-- up to date @ipmap@ into system config.
check :: IO ()
check               = do
    scf <- getMatchedConfs sysConfFilePat
    ocf <- getMatchedConfs osConfFilePat
    pcf <- getMatchedConfs planConfFilePat
    -- TODO: Here should be list monad transformer..
    let cfs = Config
                <$> map Just pcf
                <*> scf
                <*> map Just ocf
                <*> [mempty]
                <*> [mempty]
                <*> [mempty]
                <*> [mempty]
    forM_ cfs $ \c -> do
      putStrLn $ "\nTrying with config:\n" ++ show c
      runP c loadConfigs

-- | Version option defined similar to 'helper'.
versionOpt :: Parser (a -> a)
versionOpt          = abortOption (InfoMsg version) $ mconcat
                        [ long "version"
                        , short 'v'
                        , help "Show program version"
                        , hidden
                        ]

main :: IO ()
main                = join . execParser $ info
    (helper <*> versionOpt <*> hsubparser (addCmd <> rescanCmd))
    (  fullDesc
    <> header "Simple `virsh` wrapper."
    <> progDesc (  "Another interface to virsh with"
                ++ " support for templates, config plans, etc.")
    )
  where
    addCmd :: Mod CommandFields (IO ())
    addCmd          = command "add" $ info
        (runP <$> addOptions <*> pure addVm)
        (progDesc "Define new libvirt domain and create its volumes.")
    rescanCmd :: Mod CommandFields (IO ())
    rescanCmd       = command "check" $ info
        (pure check)
        (progDesc $ "Try to load all found configs and"
                    ++ " update ipmap in system config.")

