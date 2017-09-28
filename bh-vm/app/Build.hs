{-# LANGUAGE RecordWildCards        #-}

import           Data.List
import           Development.Shake
import           Development.Shake.FilePath
import           Control.Monad
import           System.Console.GetOpt
import qualified System.Directory   as D

import           Pathes

-- | Source directory with configs.
srcConfDir :: FilePath
srcConfDir  = ".." </> "configs"

buildIncludesDir :: FilePath
buildIncludesDir  = "app/Build"

-- | Replace path prefix @old@ (starting and ending at path component
-- boundaries) with @new@, if matched:
--
-- >    replacePrefix old new path
replacePrefix ::   FilePath     -- ^ @Old@ path prefix to replace with.
                -> FilePath     -- ^ @New@ path prefix to substitute to.
                -> FilePath     -- ^ Path.
                -> FilePath     -- ^ Resulting path.
replacePrefix old new x  = maybe x (combine new . joinPath) $
    -- For ensuring that path prefix starts and ends at path components
    -- (directories) boundaries, i first split them.
    stripPrefix (splitDirectories old) (splitDirectories x)

data Options =
    Options
        { -- | Prefix path used to construct all others pathes.
          _prefix       :: FilePath
        -- | Prefix path used to construct '_bindir'. If it's empty, '_prefix'
        -- will be used.
        , _execPrefix   :: FilePath
        -- | Directory for installing binary files.
        , _bindir       :: FilePath
        -- | Directory for installing config files.
        , _sysconfdir   :: FilePath
        , _flags        :: [String]
        }
  deriving (Show)

-- | Default 'Options'.
defOptions :: Options
defOptions          =
    Options
        { _prefix       = "t"
        , _execPrefix   = ""
        , _bindir       = "bin"
        , _sysconfdir   = "etc" </> "bh-vm"
        , _flags        = []
        }

-- | Construct full config installation path by applying '_sysconfdir' to
-- '_prefix'.
sysconfdir :: Options -> FilePath
sysconfdir Options {..} = _prefix </> _sysconfdir

-- | Construct full binary installation path by applying '_bindir' to
-- '_execPrefix', if not empty, or '_prefix' otherwise.
bindir :: Options -> FilePath
bindir Options {..}
  | null _execPrefix    = _prefix       </> _bindir
  | otherwise           = _execPrefix   </> _bindir

-- | 'OptDescr' for 'shakeArgsWith' (and 'getOpt').
opts :: [OptDescr (Either String (Options -> Options))]
opts                =
    [ Option [] ["prefix"]
        (ReqArg (\x -> Right (\op -> op{_prefix = x})) "PATH")
        ("Prefix path used to construct all other pathes."
            ++ " Default: '" ++ _prefix defOptions ++ "'.")
    , Option [] ["exec-prefix"]
        (ReqArg (\x -> Right (\op -> op{_execPrefix = x})) "PATH")
        ("Prefix path used to construct 'bindir' path."
            ++ " If not specified, 'prefix' will be used."
            ++ " Default: '" ++ _execPrefix defOptions ++ "'.")
    , Option [] ["bindir"]
        (ReqArg (\x -> Right (\op -> op{_bindir = x})) "PATH")
        ("Binary installation path under prefix directory."
            ++ " '--prefix' or '--exec-prefix' are prepended to this path."
            ++ " Default: '" ++ bindir defOptions ++ "'.")
    , Option [] ["sysconfdir"]
        (ReqArg (\x -> Right (\op -> op{_sysconfdir = x})) "PATH")
        ("Config files installation directory under prefix."
            ++ " '--prefix' is prepended to this path."
            ++ " Default: '" ++ sysconfdir defOptions ++ "'.")
    , Option [] ["flag"]
        (ReqArg (\x -> Right (\op -> op{_flags = x : _flags op})) "FLAG")
        "Pass flags to `stack`."
    ]

-- | Build rules.
build :: Options -> [String] -> Rules ()
build op@Options{..} args       = do
    let flags = if null _flags
                    then []
                    else "--flag" : intersperse "--flag" _flags
    if null args
      then want ["all"]
      else want args

    "all"       ~> need ["build"]

    "install"   ~> do
        need ["build"]
        needConfigs planConfFilePat
        needConfigs osConfFilePat
        needConfigs sysConfFilePat
        needConfigs tmplConfFilePat
        need [bindir op </> "bh-vm"]

    "build"     ~> do
        need [buildIncludesDir </> "Pathes.hs"]
        cmd "stack build" flags

    "clean"     ~> do
        liftIO $ removeFiles buildIncludesDir ["//"]
        cmd "stack clean"

    -- Generate module file with used config prefix.
    buildIncludesDir </> "*.hs" %> \dst ->
        writeFileChanged dst $ "module Build.Pathes\n\
            \  where\n\
            \   prefix :: FilePath\n\
            \   prefix = \"" ++ sysconfdir op ++ "\"\n"

    -- Install config files.
    alternatives $ do
      -- Certain files i don't want to overwrite once they're installed.
      installMissing sysConfFilePat
      -- Others i will overwrite with newer versions.
      instConfDir ++ "//*" %> \dst ->
        copyFile' (replacePrefix instConfDir srcConfDir dst) dst

    -- Install binary files.
    bindir op ++ "//*" %> \dst -> do
        Stdout stackInstallRoot <- cmd "stack path --local-install-root"
        when (null stackInstallRoot) $
            error "Can't find stack local install root."
        let srcBinDir = takeWhile (/= '\n') stackInstallRoot </> "bin"
        copyFile' (replacePrefix (bindir op) srcBinDir dst) dst
  where
    instConfDir :: FilePath
    instConfDir = sysconfdir op
    -- | Find config files and 'need' them.
    needConfigs :: (FilePath -> (FilePath, [FilePattern])) -> Action ()
    needConfigs f   = do
        let (d, ps) = f srcConfDir
        fs <- getDirectoryFiles d ps
        let rs = map (replacePrefix srcConfDir instConfDir d </>) fs
        need rs
    installMissing :: (FilePath -> (FilePath, [FilePattern])) -> Rules ()
    installMissing f = do
        let (d, ps) = f instConfDir
        map (normaliseEx (d ++ "/") ++) ps |%> \dst -> do
          b <- liftIO (D.doesFileExist dst)
          unless b $ copyFile' (replacePrefix instConfDir srcConfDir dst) dst

main :: IO ()
main                = shakeArgsWith shakeOptions opts $ \fopts args ->
                        let op = foldr (.) id fopts defOptions
                        in  return (Just (build op args))
