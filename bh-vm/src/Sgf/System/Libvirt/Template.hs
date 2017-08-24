{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}

module Sgf.System.Libvirt.Template
    ( showParserError
    , printParserError
    , loadFile
    , genXml
    , volumeLookup
    , domainLookup
    , genVolumeXml
    , genDomainXml
    )
  where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Control.Monad.Except
import Text.Ginger
import TextShow
import System.IO.Error
import qualified Filesystem.Path.CurrentOS as F
import Control.Monad.Writer

import Sgf.Common
import Sgf.System.Libvirt.Types


-- | Convert 'ParserError' to an error message.
showParserError :: Source -> ParserError -> String
showParserError src ParserError{..} =
    let e  = "Ginger parser error" ++ showFileName ++ ": " ++ peErrorMessage
        es = showErrorPos (peSourceLine, peSourceColumn)
    in  unlines $ e : es
  where
    showFileName :: String
    showFileName    = maybe "" (\s -> " in file '" ++ s ++ "'") peSourceName
    showErrorPos :: (Maybe Int, Maybe Int) -> [String]
    showErrorPos (Just l, Just c) =
        let ln = take 1 . drop (l - 1) . lines $ src
        in  case ln of
              []    -> []
              x : _ -> [x, replicate (c - 1) ' ' ++ "^"]
    showErrorPos (_, _) = []

-- | Use 'IncludeResolver' to obtain source file content and convert
-- 'ParserError' to an error message.
printParserError :: Monad m => IncludeResolver m -> ParserError -> m String
printParserError res pe = do
    let mfn = peSourceName pe
    src <- maybe (return "") (fmap (fromMaybe "") . res) mfn
    return $ showParserError src pe

-- | Wrapper around 'readFile' to be used as 'IncludeResolver'.
loadFile :: MonadIO m => IncludeResolver m
loadFile fn         = liftIO $ tryIOError (readFile fn) >>= \e ->
                        case e of
                          Right contents -> return (Just contents)
                          Left err       -> print err >> return Nothing

-- | Read template from file, generate xml with specified context lookup
-- function and write it as 'Text' to destination file.
genXml :: MonadIO m =>
             (VarName -> GVal (Run (Writer T.Text) T.Text))
          -> F.FilePath -> m (Either ParserError T.Text)
genXml scope src    = do
    etpl <- parseGingerFile loadFile (F.encodeString src)
    return $ runGinger (makeContextText scope) <$> etpl

-- | Context lookup function using 'Volume' as a source.
volumeLookup :: Monad m => Volume -> VarName -> GVal m
volumeLookup Volume{..} n
  | n == "name"     = toGVal . showt $ volName
  | n == "size"     = toGVal . showt $ fromLast volSize
  | otherwise       = error . T.unpack $ "No such variable: '" <> n <> "'"

-- | Context lookup function using 'Domain' as a source.
domainLookup :: Monad m => Domain -> VarName -> GVal m
domainLookup Domain{..} n
  | n == "name"     = toGVal . showt $ name
  | n == "arch"     = toGVal . showt $ arch
  | n == "memory"   = toGVal . showt $ fromLast memory
  | n == "vcpu"     = toGVal . showt $ fromLast vcpu
  | n == "bridge"   = toGVal . showt $ intName (fromLast bridge)
  | n == "ip"       = toGVal . showt $ fromLast ip
  | n == "cdrom"    = toGVal . showt $ fromFirst cdrom
  | otherwise       = error . T.unpack $ "No such variable: '" <> n <> "'"

-- | Generate libvirt volume xml using specified 'Volume' value as
-- 'GingerContext' lookup source.
genVolumeXml :: MonadIO m => Volume -> F.FilePath -> m (Either ParserError T.Text)
genVolumeXml v src  = genXml (volumeLookup v) src

-- | Generate libvirt domain xml using specified 'Domain' value as
-- 'GingerContext' lookup source.
genDomainXml :: MonadIO m => Domain -> F.FilePath -> m (Either ParserError T.Text)
genDomainXml d src  = genXml (domainLookup d) src

