{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RecordWildCards        #-}

-- |
-- Module: System.Libvirt.Template
--
-- Generate @libvirt@ xml from @jinja2@ template.

module System.Libvirt.Template
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

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                  as T
import           Control.Monad.Except
import           Text.Ginger
import           TextShow
import           System.IO.Error
import qualified Filesystem.Path.CurrentOS  as F
import           Control.Monad.Writer

import           System.Libvirt.Types

import           Internal.Common


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
  | n == "name"     = notEmptyGVal n volName
  | n == "size"     = notEmptyGVal n (fromLast volSize)
  | otherwise       = error . T.unpack $ "No such variable: '" <> n <> "'"

-- | Context lookup function using 'Domain' as a source.
domainLookup :: Monad m => Domain -> VarName -> GVal m
domainLookup Domain{..} n
  | n == "name"     = notEmptyGVal n name
  | n == "arch"     = notEmptyGVal n arch
  | n == "memory"   = notEmptyGVal n (fromLast memory)
  | n == "vcpu"     = notEmptyGVal n (fromLast vcpu)
  | n == "bridge"   = notEmptyGVal n (intName (fromLast bridge))
  | n == "ip"       = notEmptyGVal n (fromLast ip)
  | n == "cdrom"    = notEmptyGVal n (fromFirst cdrom)
  | n == "path"     = notEmptyGVal n (fromFirst (volPath volume))
  | otherwise       = error . T.unpack $ "No such variable: '" <> n <> "'"

-- | Check, that value is not a 'mempty'.
notEmptyGVal :: (Eq a, Monoid a, TextShow a) => VarName -> a -> GVal m
notEmptyGVal n x
  | x == mempty     = error . T.unpack $ "Variable '" <> n <> "' is empty."
  | otherwise       = toGVal . showt $ x

-- | Generate libvirt volume xml using specified 'Volume' value as
-- 'GingerContext' lookup source.
genVolumeXml :: MonadIO m => Volume -> F.FilePath -> m (Either ParserError T.Text)
genVolumeXml v src  = genXml (volumeLookup v) src

-- | Generate libvirt domain xml using specified 'Domain' value as
-- 'GingerContext' lookup source.
genDomainXml :: MonadIO m => Domain -> F.FilePath -> m (Either ParserError T.Text)
genDomainXml d src  = genXml (domainLookup d) src

