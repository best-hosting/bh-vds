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
import Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as F (empty)

import Sgf.System.Libvirt
import Sgf.Data.Generics.Lenses


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
checkIp x           = (x ==) . _domIp

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

