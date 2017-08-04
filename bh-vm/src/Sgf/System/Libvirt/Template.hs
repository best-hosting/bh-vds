{-# LANGUAGE OverloadedStrings       #-}

module Sgf.System.Libvirt.Template
    ( throwParserError
    , throwParserError'
    , parseGingerFileE
    , genXml
    , volumeLookup
    , domainLookup
    , genVolumeXml
    , genDomainXml
    )
  where

import Data.Maybe
import Data.String
import Data.Monoid
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Except
import Text.Ginger
import TextShow
import System.IO.Error
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad.Writer

import Sgf.System.Libvirt.Types


-- | Convert 'ParserError' to an error message and throw it in 'MonadError'.
throwParserError :: (IsString e, MonadError e m) =>
                    String -> ParserError -> m a
throwParserError src pe =
    let e  = "Ginger parser error: " ++ peErrorMessage pe
        es = showErrorPos (peSourceLine pe, peSourceColumn pe)
    in  throwError . S.fromString . unlines $ e : es
  where
    showErrorPos :: (Maybe Int, Maybe Int) -> [String]
    showErrorPos (Just l, Just c) =
        let ln = take 1 . drop (l - 1) . lines $ src
        in  case ln of
              []    -> []
              x : _ -> [x, replicate (c - 1) ' ' ++ "^"]
    showErrorPos (_, _) = []

-- | Version of 'throwParserError', which uses 'IncludeResolver' to obtain
-- source file content.
throwParserError' :: (IsString e, MonadError e m) =>
                     IncludeResolver m -> ParserError -> m a
throwParserError' res pe = do
    let mfn = peSourceName pe
    src <- maybe (return "") (fmap (fromMaybe "") . res) mfn
    throwParserError src pe

-- | Wrapper around 'readFile' to match with 'IncludeResolver' type.
loadFile :: MonadIO m => IncludeResolver m
loadFile fn         = liftIO $ tryIOError (readFile fn) >>= \e ->
                        case e of
                          Right contents -> return (Just contents)
                          Left err       -> print err >> return Nothing

-- | 'parseGingerFile' version in 'MonadError'.
parseGingerFileE :: (IsString e, MonadError e m) => IncludeResolver m -> SourceName -> m Template
parseGingerFileE res src =
    parseGingerFile res src >>= either (throwParserError' res) return

-- | Read template from file, generate xml with specified context lookup
-- function and write it as 'Text' to destination file.
genXml :: (IsString e, MonadError e m, MonadIO m) =>
          (VarName -> GVal (Run (Writer T.Text) T.Text)) -> F.FilePath -> F.FilePath -> m ()
genXml scope src dst  = do
    tpl <- parseGingerFileE loadFile (F.encodeString src)
    let h = runGinger (makeContextText scope) tpl
    liftIO $ T.writeFile (F.encodeString dst) h

-- | Context lookup function using 'Volume' as a source.
volumeLookup :: Monad m => Volume -> VarName -> GVal m
volumeLookup v n
  | n == "name"     = toGVal . showt . _volName $ v
  | n == "size"     = toGVal . showt . _volSize $ v
  | otherwise       = error . T.unpack $ "No such variable: " <> n

-- | Context lookup function using 'Domain' as a source.
domainLookup :: Monad m => Domain -> VarName -> GVal m
domainLookup d n
  | n == "name"     = toGVal . showt . _name $ d
  | n == "arch"     = toGVal . showt . _arch $ d
  | n == "memory"   = toGVal . showt . _memory $ d
  | n == "vcpu"     = toGVal . showt . _vcpu $ d
  | n == "bridge"   = toGVal . showt . _intName . _bridge $ d
  | n == "ip"       = toGVal . showt . _ip $ d
  | otherwise       = error . T.unpack $ "No such variable: " <> n

-- | Generate libvirt volume xml using specified 'Volume' value as
-- 'GingerContext' lookup source.
genVolumeXml :: (IsString e, MonadError e m, MonadIO m) =>
                Volume -> F.FilePath -> F.FilePath -> m ()
genVolumeXml v src dst  = genXml (volumeLookup v) src dst

-- | Generate libvirt domain xml using specified 'Domain' value as
-- 'GingerContext' lookup source.
genDomainXml :: (IsString e, MonadError e m, MonadIO m) =>
                Domain -> F.FilePath -> F.FilePath -> m ()
genDomainXml d src dst  = genXml (domainLookup d) src dst

