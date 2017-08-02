{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  where

import Data.Maybe
import Prelude      hiding (FilePath)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Options.Applicative
import Control.Applicative
import Text.XML.Light
import Data.Generics
import Control.Monad.Reader
import Data.Monoid
import Data.String
import Control.Arrow
import Control.Monad.Except
import Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as F (empty)

import Sgf.System.Libvirt
import Sgf.Data.Generics.Lenses
import Sgf.System.Libvirt.Template


{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}


class FromXML a where
    fromXML :: [Content] -> a


{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}

checkIp :: IP -> Domain -> Bool
checkIp x           = (x ==) . _ip

t :: Volume
t   = mempty
        { _volName = either error id $ parseName "abc"
        , _volSize = either error id $ parseSize "1"
        }

d :: Domain
d   = mempty
        { _name = either error id $ parseName "dddd"
        , _arch = either error id $ parseArch "x86_64"
        , _vcpu = either error id $ parseVCpu "3"
        , _memory = either error id $ parseSize "2886"
        , _bridge = either error (($ mempty) . appEndo) $ parseIntName "eeeeth"
        , _ip = either error id $ parseIP "9.8.7.6"
        }

genTmpl2 :: (MonadIO m) => m ()
genTmpl2    = do
    r <- runExceptT $ do
      genVolumeXml t "../vol.xml" "./vol-gen.xml"
      genDomainXml d "../dom.xml" "./dom-gen.xml"
    case r of
      Left e -> liftIO $ putStr e
      _      -> return ()

-- T.readFile "../volume.xml" >>= return . (\x -> gmapT (id `extT` volNameT x `extT` volSizeT x) defVolume) . parseXML
main :: IO ()
main                = do
    cd <- T.readFile "../test4-dom.xml"
    let dom = readDomainXml cd
        --d = ($ dom) . appEndo $ domVolume vol
    print dom
    vs <- forM (_volume dom) $ \v -> do
      let --p  = everything (<|>) (Nothing `mkQ` (Just . _volPath)) dom
          vf = "../" </> basename (fromJust . getAlt $ _volPath v) <.> "xml"
      cv <- T.readFile (encodeString vf)
      let vol = readVolumeXml cv
      print vol
      return (v <> vol)
    let dom2 = set vs dom
    print dom2
    d3 <- initDomain (\p -> T.readFile (encodeString ("../" </> basename p <.> "xml"))) cd
    print (d3 == dom2)
    {-cv <- T.readFile "../vol.xml"
    let vol2 = readVolumeXml cv
    print vol2-}

