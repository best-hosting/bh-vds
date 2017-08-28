{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Sgf.System.Libvirt.Types
    ( Name
    , parseName
    , Size
    , parseSize
    , Path (..)
    , Pool (..)
    , Volume (..)
    , volNameL
    , volSizeL
    , volPathL
    , volPoolL

    , Arch
    , parseArch
    , VCpu
    , parseVCpu
    , Interface (..)
    , IP
    , parseIP
    , Domain (..)
    , nameL
    , archL
    , memoryL
    , vcpuL
    , cdromL
    , volumeL
    , bridgeL
    , ipL

    , VmError (..)
    , toVmError
    , toVmErrorM
    , liftVmError
    )
  where

import Data.Maybe
import qualified Data.String as S
import Data.Monoid
import Data.Generics
import qualified Text.Ginger as G
import Data.Yaml.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import TextShow
import Control.Applicative
import Control.Monad.Except
import qualified Filesystem.Path.CurrentOS as F

import Sgf.Control.Lens


-- | Type for names. Use 'parseName' for converting from text and 'showt' for
-- converting back to text.
newtype Name        = Name {getName :: T.Text}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

-- | Parser for 'Name'.
parseName :: T.Text -> Either String Name
parseName           = Right . Name

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally. I shouldn't use 'showb' for 'Text' here (instead of
-- 'fromText'), because it adds double quotes around text, but i don't need
-- them.
instance TextShow Name where
    showb           = fromText . getName
instance S.IsString Name where
    fromString      = either error id . parseName . T.pack
instance FromJSON Name where
    parseJSON v     = parseJSON v >>= either fail pure . parseName
instance ToJSON Name where
    toJSON          = toJSON . showt

-- | Type for sizes. Use 'parseSize' for converting from text, 'fromInteger'
-- for converting from a number and 'toInteger' for converting to a number.
-- Not all 'Integer's are valid 'Size's, thus i don't provide lens
-- 'LensA Size Integer', because such lens will break lens laws.
newtype Size        = Size {getSize :: Sum Integer}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

-- | Convert 'Integer' to 'Size' returning an error, if integer is out of
-- bounds.
toSize :: Integer -> Either String Size
toSize x
  | x >= 0          = return $ Size (Sum x)
  | otherwise       = Left $ "Negative 'Size' '" <> show x <> "'."
-- | Convert 'Integer' to 'Size' returning 0 on out of bound errors.
toSize' :: Integer -> Size
toSize'             = either (const (Size mempty)) id . toSize

-- | Parser for 'Size'.
parseSize :: T.Text -> Either String Size
parseSize           = A.parseOnly $ A.decimal >>= either fail pure . toSize

-- | Substraction rounds negative size to 0. But 'fromInteger' fails with
-- runtime error, when called on negative value.
instance Num Size where
    Size x + Size y = Size (x + y)
    Size x - Size y = toSize' $ getSum (x - y)
    Size x * Size y = Size (x * y)
    abs (Size x)    = Size (abs x)
    signum (Size x) = Size (signum x)
    fromInteger     = either error id . toSize
instance Enum Size where
    toEnum          = fromInteger . toInteger
    fromEnum        = fromInteger . toInteger
instance Real Size where
    toRational      = toRational . toInteger
instance Integral Size where
    x `quotRem` y   = let x' = toInteger $ x
                          y' = toInteger $ y
                          (q, r) = x' `quotRem` y'
                      in  (fromInteger q, fromInteger r)
    toInteger       = getSum . getSize

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow Size where
    showb           = fromText . showt . toInteger
instance FromJSON Size where
    parseJSON v     = parseJSON v >>= either fail pure . toSize
instance ToJSON Size where
    toJSON          = toJSON . toInteger

-- | 'FilePath' 'Monoid' instance is wrong (it does not satisfy 'Monoid'
-- laws), thus i need a wrapper to fix it. See
-- https://github.com/fpco/haskell-filesystem/issues/19 .
newtype Path        = Path {getPath :: F.FilePath}
  deriving (Show, Typeable, Data, Eq, Ord)

instance Monoid Path where
    mempty          = Path mempty
    Path x `mappend` Path y
      | x == mempty = Path y
      | y == mempty = Path x
      | otherwise   = Path (x `mappend` y)
instance TextShow Path where
    showb           = fromString . F.encodeString . getPath
instance FromJSON Path where
    parseJSON v     = Path . F.decodeString <$> parseJSON v
instance ToJSON Path where
    toJSON          = toJSON . F.encodeString . getPath

-- | Type for libvirt storage pool.
newtype Pool        = Pool {poolName :: Name}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

instance TextShow Pool where
    showb           = showb . poolName
instance FromJSON Pool where
    parseJSON v     = Pool <$> parseJSON v
instance ToJSON Pool where
    toJSON          = toJSON . poolName

-- | Type for libvirt storage volume.
data Volume         = Volume
                        { volName  :: Name          -- ^ Volume @name@.
                        , volSize  :: Last Size     -- ^ Volume @capacity@.
                        , volPath  :: First Path    -- ^ Volume @<target><path/>@.
                        , volPool  :: Last Pool     -- ^ Volume pool.
                        }
  deriving (Show, Typeable, Data, Eq, Ord)

instance Monoid Volume where
    mempty          = Volume
                        { volName   = mempty
                        , volSize   = mempty
                        , volPath   = mempty
                        , volPool   = mempty
                        }
    x `mappend` y   = Volume
                        { volName   = volName x <> volName y
                        , volSize   = volSize x <> volSize y
                        , volPath   = volPath x <> volPath y
                        , volPool   = volPool x <> volPool y
                        }

instance FromJSON Volume where
    parseJSON           = withObject "Volume" $ \o -> Volume
                            <$> o .:? "name" .!= mempty -- _volName
                            <*> o .:? "size" .!= mempty -- _volSize
                            <*> o .:? "path" .!= mempty -- _volPath
                            <*> o .:? "pool" .!= mempty -- _pool
instance ToJSON Volume where
    toJSON Volume{..}   = object . catMaybes $
                            [ "name" .=? volName
                            , "size" .=? volSize
                            , "path" .=? volPath
                            , "pool" .=? volPool
                            ]

volNameL :: LensA Volume Name
volNameL f z@Volume {volName = x}   = fmap (\x' -> z{volName = x'}) (f x)
volSizeL :: LensA Volume (Last Size)
volSizeL f z@Volume {volSize = x}   = fmap (\x' -> z{volSize = x'}) (f x)
volPathL :: LensA Volume (First Path)
volPathL f z@Volume {volPath = x}   = fmap (\x' -> z{volPath = x'}) (f x)
volPoolL :: LensA Volume (Last Pool)
volPoolL f z@Volume {volPool = x}   = fmap (\x' -> z{volPool = x'}) (f x)

-- | Type for architecture. Use 'parseArch' for converting from text and
-- 'showt' for converting back to text.
newtype Arch        = Arch {getArch :: Last T.Text}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

-- | Parser for 'Arch'.
parseArch :: T.Text -> Either String Arch
parseArch           = A.parseOnly $ Arch . Last . Just
                        <$> (A.string "x86_64" <|> A.string "i686")

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow Arch where
    showb           = fromText . fromMaybe "" . getLast . getArch
instance FromJSON Arch where
    parseJSON v     = parseJSON v >>= either fail pure . parseArch
instance ToJSON Arch where
    toJSON          = toJSON . showt

-- | Type for libvirt @vcpu@ definition inside domain. Use 'parseVCpu' for
-- converting from text, 'fromInteger' for converting from a number and
-- 'toInteger' for converting to a number. Not all 'Integer's are valid
-- 'VCpu's, hence i don't provide lens 'LensA VCpu Integer', because such lens
-- will break lens laws.
newtype VCpu        = VCpu {getVCpu :: Sum Integer}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

-- | Convert 'Integer' to 'VCpu' returning an error, if integer is out of
-- bounds.
toVCpu :: Integer -> Either String VCpu
toVCpu x
  | x >= 0          = return $ VCpu (Sum x)
  | otherwise       = Left $ "Negative or zero 'VCpu' '" <> show x <> "'."
-- | Convert 'Integer' to 'VCpu' returning 0 on out of bound errors.
toVCpu' :: Integer -> VCpu
toVCpu'             = either (const (VCpu mempty)) id . toVCpu

-- | Parser for 'VCpu'.
parseVCpu :: T.Text -> Either String VCpu
parseVCpu           = A.parseOnly $ A.decimal >>= either fail pure . toVCpu

-- | Substraction rounds negative size to 0. But 'fromInteger' fails with
-- runtime error, when called on negative value.
instance Num VCpu where
    VCpu x + VCpu y = VCpu (x + y)
    VCpu x - VCpu y = toVCpu' $ getSum (x - y)
    VCpu x * VCpu y = VCpu (x * y)
    abs (VCpu x)    = VCpu (abs x)
    signum (VCpu x) = VCpu (signum x)
    fromInteger     = either error id . toVCpu
instance Enum VCpu where
    toEnum          = fromInteger . toInteger
    fromEnum        = fromInteger . toInteger
instance Real VCpu where
    toRational      = toRational . toInteger
instance Integral VCpu where
    x `quotRem` y   = let x' = toInteger $ x
                          y' = toInteger $ y
                          (q, r) = x' `quotRem` y'
                      in  (fromInteger q, fromInteger r)
    toInteger       = getSum . getVCpu

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow VCpu where
    showb           = fromText . showt . toInteger
instance FromJSON VCpu where
    parseJSON v     = parseJSON v >>= either fail pure . toVCpu
instance ToJSON VCpu where
    toJSON          = toJSON . toInteger

-- | Type for libvirt network @interface@ definition inside domain.
newtype Interface   = Interface {intName :: Name}
  deriving (Show, Typeable, Data, Eq, Ord, Monoid)

instance FromJSON Interface where
    parseJSON v     = Interface <$> parseJSON v
instance ToJSON Interface where
    toJSON Interface{..}    = toJSON intName

-- | Type for IP address.
data IP             = IP    { _octet1 :: Sum Int
                            , _octet2 :: Sum Int
                            , _octet3 :: Sum Int
                            , _octet4 :: Sum Int
                            }
  deriving (Show, Typeable, Data, Eq, Ord)

instance Monoid IP where
    mempty          = IP mempty mempty mempty mempty
    x `mappend` y   = IP    { _octet1 = _octet1 x `mappend` _octet1 y
                            , _octet2 = _octet2 x `mappend` _octet2 y
                            , _octet3 = _octet3 x `mappend` _octet3 y
                            , _octet4 = _octet4 x `mappend` _octet4 y
                            }

-- | Parser for 'IP'.
parseIP :: T.Text -> Either String IP
parseIP             = A.parseOnly $
                        IP <$> octet <*> octet <*> octet <*> octet
  where
    octet :: A.Parser (Sum Int)
    octet           = do
        x <- A.decimal <* (A.string "." <|> A.endOfInput *> pure "")
        case x of
          _
            | x > 255   -> error $
                "Octet " ++ show x ++ " is too great."
            | x < 0     -> error $
                "Impossible happens: negative octet " ++ show x
            | otherwise -> pure (Sum x)

instance TextShow IP where
    showb x         = fromText $    (showt . getSum . _octet1 $ x)
                        <>  "." <>  (showt . getSum . _octet2 $ x)
                        <>  "." <>  (showt . getSum . _octet3 $ x)
                        <>  "." <>  (showt . getSum . _octet4 $ x)

instance FromJSON IP where
    parseJSON v     = parseJSON v >>= either fail pure . parseIP
instance ToJSON IP where
    toJSON          = toJSON . showt

-- | Type for libvirt @domain@.
data Domain         = Domain
                        { name      :: Name
                        , arch      :: Arch
                        , memory    :: Last Size
                        , vcpu      :: Last VCpu
                        , cdrom     :: First Path
                        , volume    :: Volume
                        , bridge    :: Last Interface
                        , ip        :: Last IP
                        }
  deriving (Show, Typeable, Data, Eq, Ord)

nameL :: LensA Domain Name
nameL   f z@Domain {name = x}   = fmap (\x' -> z{name = x'}) (f x)
archL :: LensA Domain Arch
archL   f z@Domain {arch = x}   = fmap (\x' -> z{arch = x'}) (f x)
memoryL :: LensA Domain (Last Size)
memoryL f z@Domain {memory = x} = fmap (\x' -> z{memory = x'}) (f x)
vcpuL :: LensA Domain (Last VCpu)
vcpuL   f z@Domain {vcpu = x}   = fmap (\x' -> z{vcpu = x'}) (f x)
cdromL :: LensA Domain (First Path)
cdromL  f z@Domain {cdrom = x}  = fmap (\x' -> z{cdrom = x'}) (f x)
volumeL :: LensA Domain Volume
volumeL f z@Domain {volume = x} = fmap (\x' -> z{volume = x'}) (f x)
bridgeL :: LensA Domain (Last Interface)
bridgeL f z@Domain {bridge = x} = fmap (\x' -> z{bridge = x'}) (f x)
ipL :: LensA Domain (Last IP)
ipL     f z@Domain {ip = x}     = fmap (\x' -> z{ip = x'}) (f x)

instance Monoid Domain where
    mempty          = Domain
                        { name      = mempty
                        , arch      = mempty
                        , memory    = mempty
                        , vcpu      = mempty
                        , cdrom     = mempty
                        , volume    = mempty
                        , bridge    = mempty
                        , ip        = mempty
                        }
    x `mappend` y   = Domain
                        { name      = name x        <> name y
                        , arch      = arch x        <> arch y
                        , memory    = memory x      <> memory y
                        , vcpu      = vcpu x        <> vcpu y
                        , cdrom     = cdrom x       <> cdrom y
                        , volume    = volume x      <> volume y
                        , bridge    = bridge x      <> bridge y
                        , ip        = ip x          <> ip y
                        }

instance FromJSON Domain where
    parseJSON       = withObject "Domain" $ \o -> Domain
                        <$> o .:? "name"    .!= mempty  -- name
                        <*> o .:? "arch"    .!= mempty  -- arch
                        <*> o .:? "memory"  .!= mempty  -- memory
                        <*> o .:? "vcpu"    .!= mempty  -- vcpu
                        <*> o .:? "cdrom"   .!= mempty  -- cdrom
                        <*> o .:? "volume"  .!= mempty  -- _volume
                        <*> o .:? "bridge"  .!= mempty  -- interface
                        <*> o .:? "ip"      .!= mempty  -- ip
instance ToJSON Domain where
    toJSON Domain{..}   = object . catMaybes $
                            [ "name"    .=? name
                            , "arch"    .=? arch
                            , "memory"  .=? memory
                            , "vcpu"    .=? vcpu
                            , "cdrom"   .=? cdrom
                            , "volume"  .=? volume
                            , "bridge"  .=? bridge
                            , "ip"      .=? ip
                            ]

infixr 8 .=?
(.=?) :: (Eq v, Monoid v, ToJSON v, KeyValue kv) => T.Text -> v -> Maybe kv
t .=? x
  | x == mempty     = Nothing
  | otherwise       = Just (t .= x)

optionA :: (Monoid a, Alternative f) => f a -> f a
optionA             = A.option mempty

data VmError        = XmlGenError G.ParserError
                    | YamlParseError F.FilePath ParseException
                    | LibvirtError T.Text
                    | UnknownError TypeRep
  deriving (Show)

-- | Convert some error to 'VmError'. Note, that 'XmlGenerationError' value
-- can't be constructed using this function, because it contains
-- 'IncludeResolver'.
toVmError :: Typeable a => a -> VmError
toVmError x         = fromMaybe (UnknownError (typeOf x)) $
        uncurry YamlParseError  <$> cast x
    <|>         XmlGenError     <$> cast x
    <|>         LibvirtError    <$> cast x

toVmErrorM :: (Functor m, Typeable e) => ExceptT e m a -> ExceptT VmError m a
toVmErrorM          = withExceptT toVmError

liftVmError :: (Typeable e, MonadError VmError m) => m (Either e a) -> m a
liftVmError m       = m >>= either (throwError . toVmError) return

