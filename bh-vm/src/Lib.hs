{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Lib
  where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Options.Applicative
import Control.Applicative
import Text.XML.Light
import Data.Generics
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid


{-data Plan           = Plan {size :: Int, memory :: Int, cpu :: Int}
data Template       = Template {cdrom :: FilePath}
data System         = System {pool :: Text, bridge :: Text}-}

class FromXML a where
    fromXML :: [Content] -> a

data Volume         = Volume
                        { _volName  :: T.Text
                        , _volSize  :: Integer
                        , _volPath  :: FilePath
                        , _pool     :: T.Text
                        }
  deriving (Show)
defVol :: Volume
defVol              = Volume
                        { _volName  = ""
                        , _volSize  = 0
                        , _volPath  = ""
                        , _pool     = ""
                        }

volName :: GenericQ (Endo Volume)
volName             = setName . T.concat <$> getCData [qN "name", qN "volume"]
  where setName xs  = Endo (\vol -> vol{_volName = xs})
volSize :: GenericQ (Endo Volume)
volSize             = setSize . T.unpack . T.concat
                        <$> getCData [qN "capacity", qN "volume"]
  where setSize x   = Endo (\vol -> vol{_volSize = read x})

data Interface      = Interface
                        { _ip       :: T.Text
                        , _bridge   :: T.Text
                        }
  deriving (Show)
defInterface :: Interface
defInterface        = Interface
                        { _ip       = ""
                        , _bridge   = ""
                        }

data Domain         = Domain
                        { name      :: T.Text
                        , arch      :: T.Text
                        , memory    :: Int
                        , cpu       :: Int
                        , cdrom     :: Maybe FilePath
                        , interface :: Interface
                        , volume    :: Volume
                        }
  deriving (Show)

t :: Content
t = Elem (Element {elName = QName {qName = "volume", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "block"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 1}),Elem (Element {elName = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "vm-{{name}}", cdLine = Just 2})], elLine = Just 2}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 2}),Elem (Element {elName = QName {qName = "capacity", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "unit", qURI = Nothing, qPrefix = Nothing}, attrVal = "G"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "14", cdLine = Just 3})], elLine = Just 3}),Text (CData {cdVerbatim = CDataText, cdData = "\n", cdLine = Just 3})], elLine = Just 1})

t2 :: Content
t2 = Elem (Element {elName = QName {qName = "volume", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "block"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 1}),Elem (Element {elName = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "vm-{{name}}", cdLine = Just 2})], elLine = Just 2}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 2}),Elem (Element {elName = QName {qName = "capacity", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "unit", qURI = Nothing, qPrefix = Nothing}, attrVal = "G"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 3}),Elem (Element {elName = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "vm-abc", cdLine = Just 4})], elLine = Just 4}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 4}),Elem (Element {elName = QName {qName = "test", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "{{size}}", cdLine = Just 5})], elLine = Just 5}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 5})], elLine = Just 3}),Text (CData {cdVerbatim = CDataText, cdData = "\n", cdLine = Just 6})], elLine = Just 1})

t3 :: Element
t3 = (Element {elName = QName {qName = "volume", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "block"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "vm-abc", cdLine = Just 4})], elLine = Just 1})

t4 :: [Content]
t4  = [Elem (Element {elName = QName {qName = "domain", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "kvm"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 1}),Elem (Element {elName = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "{{name}}", cdLine = Just 2})], elLine = Just 2}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 2}),Elem (Element {elName = QName {qName = "memory", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "unit", qURI = Nothing, qPrefix = Nothing}, attrVal = "MiB"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "{{memory}}", cdLine = Just 3})], elLine = Just 3}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 3}),Elem (Element {elName = QName {qName = "vcpu", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "placement", qURI = Nothing, qPrefix = Nothing}, attrVal = "static"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "{{cpu}}", cdLine = Just 4})], elLine = Just 4}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 4}),Elem (Element {elName = QName {qName = "os", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 5}),Elem (Element {elName = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "arch", qURI = Nothing, qPrefix = Nothing}, attrVal = "{{arch}}"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "hvm", cdLine = Just 6})], elLine = Just 6}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 6})], elLine = Just 5}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 7}),Elem (Element {elName = QName {qName = "devices", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 8}),Elem (Element {elName = QName {qName = "emulator", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "/usr/bin/kvm", cdLine = Just 9})], elLine = Just 9}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 9}),Elem (Element {elName = QName {qName = "disk", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "block"},Attr {attrKey = QName {qName = "device", qURI = Nothing, qPrefix = Nothing}, attrVal = "disk"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 10}),Elem (Element {elName = QName {qName = "driver", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, attrVal = "qemu"},Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "raw"},Attr {attrKey = QName {qName = "cache", qURI = Nothing, qPrefix = Nothing}, attrVal = "none"},Attr {attrKey = QName {qName = "io", qURI = Nothing, qPrefix = Nothing}, attrVal = "native"}], elContent = [], elLine = Just 11}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 11}),Elem (Element {elName = QName {qName = "source", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "dev", qURI = Nothing, qPrefix = Nothing}, attrVal = "{{volpath}}"}], elContent = [], elLine = Just 12}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 12}),Elem (Element {elName = QName {qName = "target", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "dev", qURI = Nothing, qPrefix = Nothing}, attrVal = "vda"},Attr {attrKey = QName {qName = "bus", qURI = Nothing, qPrefix = Nothing}, attrVal = "virtio"}], elContent = [], elLine = Just 13}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 13}),Elem (Element {elName = QName {qName = "boot", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "order", qURI = Nothing, qPrefix = Nothing}, attrVal = "2"}], elContent = [], elLine = Just 14}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 14})], elLine = Just 10}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 15}),Elem (Element {elName = QName {qName = "disk", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "file"},Attr {attrKey = QName {qName = "device", qURI = Nothing, qPrefix = Nothing}, attrVal = "cdrom"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 16}),Elem (Element {elName = QName {qName = "driver", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, attrVal = "qemu"},Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "raw"}], elContent = [], elLine = Just 17}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 17}),Elem (Element {elName = QName {qName = "target", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "dev", qURI = Nothing, qPrefix = Nothing}, attrVal = "hda"},Attr {attrKey = QName {qName = "bus", qURI = Nothing, qPrefix = Nothing}, attrVal = "ide"}], elContent = [], elLine = Just 18}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 18}),Elem (Element {elName = QName {qName = "readonly", qURI = Nothing, qPrefix = Nothing}, elAttribs = [], elContent = [], elLine = Just 19}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 19}),Elem (Element {elName = QName {qName = "boot", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "order", qURI = Nothing, qPrefix = Nothing}, attrVal = "1"}], elContent = [], elLine = Just 20}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 20})], elLine = Just 16}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 21}),Elem (Element {elName = QName {qName = "interface", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "bridge"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 22}),Elem (Element {elName = QName {qName = "source", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "bridge", qURI = Nothing, qPrefix = Nothing}, attrVal = "{{bridge}}"}], elContent = [], elLine = Just 23}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 23}),Elem (Element {elName = QName {qName = "model", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "type", qURI = Nothing, qPrefix = Nothing}, attrVal = "virtio"}], elContent = [], elLine = Just 24}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 24}),Elem (Element {elName = QName {qName = "filterref", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "filter", qURI = Nothing, qPrefix = Nothing}, attrVal = "clean-traffic"}], elContent = [Text (CData {cdVerbatim = CDataText, cdData = "\n        ", cdLine = Just 25}),Elem (Element {elName = QName {qName = "parameter", qURI = Nothing, qPrefix = Nothing}, elAttribs = [Attr {attrKey = QName {qName = "name", qURI = Nothing, qPrefix = Nothing}, attrVal = "IP"},Attr {attrKey = QName {qName = "value", qURI = Nothing, qPrefix = Nothing}, attrVal = "{{ip}}"}], elContent = [], elLine = Just 26}),Text (CData {cdVerbatim = CDataText, cdData = "\n      ", cdLine = Just 26})], elLine = Just 25}),Text (CData {cdVerbatim = CDataText, cdData = "\n    ", cdLine = Just 27})], elLine = Just 22}),Text (CData {cdVerbatim = CDataText, cdData = "\n  ", cdLine = Just 28})], elLine = Just 8}),Text (CData {cdVerbatim = CDataText, cdData = "\n", cdLine = Just 29})], elLine = Just 1}),Text (CData {cdVerbatim = CDataText, cdData = "\n\n", cdLine = Just 30})]

{-
    join . execParser $ info (helper <*> (work <$> opts))
      ( fullDesc
      <> header "Print server name and sendmail_path sender address."
      <> progDesc "Print server name and aliases and php sendmail_path sender address."
      )-}

-- | Extract CData from 'Content'.
textCData :: Content -> T.Text
textCData (Text CData{cdData = s})  = T.pack s
textCData _                         = mempty

-- | Make 'QName' with only 'qName' set.
qN :: String -> QName
qN s            = QName {qName = s, qURI = Nothing, qPrefix = Nothing}

-- | 'everything' traversal in context of 'Reader' updated at each element
-- with given function.
everythingR :: (MonadReader r m, Data a) => GenericQ (r -> r)
               -> (m t -> m t -> m t) -> GenericQ (m t) -> a -> m t
everythingR f k q x = local (f x)
                        $ foldl k (q x) (gmapQ (everythingR f k q) x)

data A      = A {aN :: String, aF :: [B]}
  deriving (Show, Typeable, Data)
data B      = B {bN :: String, bF :: [C]}
  deriving (Show, Typeable, Data)
data C      = C {cN :: String}
  deriving (Show, Typeable, Data)

tA :: A
tA  = A {aN = "abc", aF = []}
tB :: B
tB  = B {bN = "def", bF = []}
tC :: C
tC  = C {cN = "ghi"}
tA2 :: A
tA2 = tA{aF = [tB {bF = [tC]}]}

tA3 :: A
tA3 = tA{aF = [tB {bN = "bbb", bF = [tC]}]}

qA :: A -> String
qA  = (\x -> let n = aN x in if n == "abc" then n else "huh")
qB :: B -> String
qB  = (\x -> let n = bN x in if n == "def" then n else "ugh")
qC :: C -> String
qC  = (\x -> let n = cN x in if n == "ghi" then n else "grr")

qABut :: A -> (String, Bool)
qABut  = (\x -> let n = aN x in if n == "abc" then (n, False) else ("huh", True))
qBBut :: B -> (String, Bool)
qBBut  = (\x -> let n = bN x in if n == "def" then (n, False) else ("ugh", True))
qCBut :: C -> (String, Bool)
qCBut  = (\x -> let n = cN x in if n == "ghi" then (n, False) else ("grr", True))

anyQ2 :: GenericRecQ String
anyQ2     = ((endQ (const "fin: ") `extRecQ'` qC) `extRecQ'` qB) `extRecQ'` qA

anyQ2But :: GenericRecQ (String, Bool)
anyQ2But  = ((end "fin" `extRecQBut'` qCBut) `extRecQBut'` qBBut) `extRecQBut'` qABut




-- | Recursive 'GenericQ' query, which updates itself at each step.
data GenericRecQ r  = RQ {unRQ :: GenericQ (r, GenericRecQ r)}

-- | Final query for 'GenericRecQ' recursive query chain. It will loop
-- forever.
endQ :: GenericQ r -> GenericRecQ r
endQ f              = RQ $ \x -> (f x, endQ f)

-- | Final query, which will prevent further data traversal in schemas like
-- 'everythingRQBut'.
end :: r -> GenericRecQ (r, Bool)
end d               = endQ (const (d, True))

end' :: Monoid r => GenericRecQ (r, Bool)
end'                = endQ (const (mempty, True))

-- | Default query to pass-through non-matching types in 'everythingBut'
-- scheme.
skip :: r -> GenericQ (r, Bool)
skip x _            = (x, False)

-- | Operator for combining recursive query chains.
extRecQ :: Typeable b => GenericQ r -> GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ g cont k    = RQ $      (\x -> (g x, extRecQ g cont k))
                        `extQ`  (\x -> (k x, cont))

extRecQ' :: (Monoid r, Typeable b) => GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ'            = extRecQ (const mempty)

extRecQBut' :: (Monoid r, Typeable b) => GenericRecQ (r, Bool) -> (b -> (r, Bool)) -> GenericRecQ (r, Bool)
extRecQBut'         = extRecQ (skip mempty)

-- | 'everything' traversal for 'GenericRecQ'.
everythingRec :: Data a => (r -> r -> r) -> GenericRecQ r -> a -> r
everythingRec k q x    =
    let (z, q') = unRQ q x
    in  foldl k z (gmapQ (everythingRec k q') x)

-- | 'everythingBut' traversal for 'GenericRecQ'.
everythingRecBut :: Data a => (r -> r -> r) -> GenericRecQ (r, Bool) -> a -> r
everythingRecBut k q x  =
    let ((z, b), q') = unRQ q x
    in  if b then z else foldl k z (gmapQ (everythingRecBut k q') x)




-- | Query each element in context of its path.
queryXMLPath :: (r -> r -> r) -> GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath k q    = flip runReader []
                        . everythingR setQName (liftA2 k) (reader <$> q)

queryXMLPath' :: Monoid r => GenericQ ([QName] -> r) -> GenericQ r
queryXMLPath'       = queryXMLPath mappend

-- | Function for modifying @Reader [QName]@ context by adding 'QName' of
-- current 'Element'.
setQName :: GenericQ ([QName] -> [QName])
setQName            = id `mkQ` ((:) . elName)

-- | Extract all CData values from content of an 'Element' matching given
-- 'QName' path.
elCData :: [QName] -> GenericQ ([QName] -> [T.Text])
elCData rp          = return [] `mkQ` elCData' rp

elCData' :: [QName] -> Element -> [QName] -> [T.Text]
elCData' rp x curp
  | rp == curp      = map textCData . elContent $ x
  | otherwise       = []

elAttrVal :: [QName] -> GenericQ ([QName] -> [T.Text])
elAttrVal rp        = return [] `mkQ` elAttrVal' rp

elAttrVal' :: [QName] -> Element -> [QName] -> [T.Text]
elAttrVal' rp x curp
  | rp == curp      = map (T.pack . attrVal) . elAttribs $ x
  | otherwise       = []

-- | All 'Element'-s with full pathes.
elementPathes :: GenericQ [(QName, [QName])]
elementPathes       = queryXMLPath' elementPath

elementPath :: GenericQ ([QName] -> [(QName, [QName])])
elementPath         = return [] `mkQ` (\x cp -> [(elName x, cp)])

getCData :: [QName] -> GenericQ [T.Text]
getCData rp         = queryXMLPath' (elCData rp)

getAttrVal :: [QName] -> GenericQ [T.Text]
getAttrVal rp       = queryXMLPath' (elAttrVal rp)

{-volNameG :: GenericQ ([QName] -> Endo Volume)
volNameG x curp     = Endo $ \vol -> vol {_volName = _volName vol <> (T.concat
                        $ elCData [qN "name", qN "volume"] x curp)}-}

{-volSizeG :: GenericQ ([QName] -> Endo Volume)
volSizeG x curp     = Endo $ \vol -> vol {_volSize = read . T.unpack . T.concat
                        $ elCData [qN "capacity"] x curp}-}

main :: IO ()
main                = do
    c <- T.readFile "../vol.xml"
    let xml = parseXML c
        t :: [T.Text]
        t = getCData [qN "name", qN "volume"] xml
        --vn = ($ defVol) . appEndo . queryXMLPath' volNameG $ xml
        vl = volSize xml <> volName xml
    --print vn
    print . ($ defVol) . appEndo $ vl

