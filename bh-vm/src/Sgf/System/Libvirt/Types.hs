{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sgf.System.Libvirt.Types
    ( Name
    , parseName
    , Size
    , parseSize
    , Pool (..)
    , Volume (..)

    , Arch
    , parseArch
    , VCpu
    , parseVCpu
    , Interface (..)
    , parseIntName
    , IP
    , parseIP
    , Domain (..)
    )
  where

import Data.Monoid
import Data.Generics
import Data.Attoparsec.Text
import qualified Data.Text as T
import TextShow
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as F


-- | Type for names.
newtype Name        = Name {getName :: Last T.Text}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'Name'.
parseName :: T.Text -> Either String Name
parseName           = Right . Name . Last . Just

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow Name where
    showb x         = fromText . maybe "" id . getLast . getName $ x

-- | Type for sizes.
newtype Size        = Size {getSize :: Sum Integer}
  deriving (Show, Typeable, Data, Eq, Ord, Num, Monoid)

-- | Parser for 'Size'.
parseSize :: T.Text -> Either String Size
parseSize           = parseOnly (Size . Sum <$> decimal)

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow Size where
    showb x         = fromText . showt . getSum . getSize $ x

-- | Type for libvirt storage pool.
newtype Pool        = Pool {_poolName :: Name}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Type for libvirt storage volume. Note, the type of '_volPath': 'FilePath'
-- 'Monoid' instance is wrong (it does not satisfy 'Monoid' laws), thus i need
-- a wrapper to fix it.
data Volume         = Volume
                        { _volName  :: Name         -- ^ Volume @name@.
                        , _volSize  :: Size         -- ^ Volume @capacity@.
                        , _volPath  :: Alt Maybe F.FilePath -- ^ Volume @target -> path@.
                        , _pool     :: Pool         -- ^ Volume pool.
                        }
  deriving (Show, Typeable, Data, Eq)

instance Monoid Volume where
    mempty          = Volume
                        { _volName  = mempty
                        , _volSize  = mempty
                        , _volPath  = mempty
                        , _pool     = mempty
                        }
    x `mappend` y   = Volume
                        { _volName  = _volName x <> _volName y
                        , _volSize  = _volSize x <> _volSize y
                        , _volPath  = _volPath x <> _volPath y
                        , _pool     = _pool x    <> _pool y
                        }

-- | Type for architecture.
newtype Arch        = Arch {getArch :: Last T.Text}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'Arch'.
parseArch :: T.Text -> Either String Arch
parseArch           = parseOnly $ Arch . Last . Just
                        <$> (string "x86_64" <|> string "i686")

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow Arch where
    showb x         = fromText . maybe "" id . getLast . getArch $ x

-- | Type for libvirt @vcpu@ definition inside domain.
newtype VCpu        = VCpu {getVCpu :: Sum Integer}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for 'VCpu'.
parseVCpu :: T.Text -> Either String VCpu
parseVCpu           = parseOnly (VCpu . Sum <$> decimal)

-- | Note, this instance does /not/ match with the 'Show' instance
-- intentionally.
instance TextShow VCpu where
    showb x         = fromText . showt . getSum . getVCpu $ x

-- | Type for libvirt network @interface@ definition inside domain.
newtype Interface   = Interface {_intName :: Name}
  deriving (Show, Typeable, Data, Eq, Monoid)

-- | Parser for '_intName'.
parseIntName :: T.Text -> Either String (Endo Interface)
parseIntName        = fmap (\x -> Endo $ \i -> i{_intName = x}) . parseName

-- | Type for IP address.
data IP             = IP    { _octet1 :: Sum Int
                            , _octet2 :: Sum Int
                            , _octet3 :: Sum Int
                            , _octet4 :: Sum Int
                            }
  deriving (Show, Typeable, Data, Eq)

instance Monoid IP where
    mempty          = IP mempty mempty mempty mempty
    x `mappend` y   = IP    { _octet1 = _octet1 x `mappend` _octet1 y
                            , _octet2 = _octet2 x `mappend` _octet2 y
                            , _octet3 = _octet3 x `mappend` _octet3 y
                            , _octet4 = _octet4 x `mappend` _octet4 y
                            }

-- | Parser for 'IP'.
parseIP :: T.Text -> Either String IP
parseIP             = parseOnly $
                        IP <$> octet <*> octet <*> octet <*> octet
  where
    octet :: Parser (Sum Int)
    octet           = do
                        x <- decimal <* (string "." <|> endOfInput *> pure "")
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

-- | Type for libvirt @domain@. Note, the type of '_cdrom': 'FilePath'
-- 'Monoid' instance is wrong (it does not satisfy 'Monoid' laws), thus i need
-- a wrapper to fix it.
data Domain         = Domain
                        { _name     :: Name
                        , _arch     :: Arch
                        , _memory   :: Size
                        , _vcpu     :: VCpu
                        , _cdrom    :: Alt Maybe F.FilePath
                        , _volume   :: [Volume]
                        , _bridge   :: Interface
                        , _ip       :: IP
                        }
  deriving (Show, Typeable, Data, Eq)

instance Monoid Domain where
    mempty          = Domain
                        { _name     = mempty
                        , _arch     = mempty
                        , _memory   = mempty
                        , _vcpu     = mempty
                        , _cdrom    = Alt Nothing
                        , _volume   = mempty
                        , _bridge   = mempty
                        , _ip       = mempty
                        }
    x `mappend` y   = Domain
                        { _name     = _name x       <> _name y
                        , _arch     = _arch x       <> _arch y
                        , _memory   = _memory x     <> _memory y
                        , _vcpu     = _vcpu x       <> _vcpu y
                        , _cdrom    = _cdrom x      <> _cdrom y
                        , _volume   = _volume x     <> _volume y
                        , _bridge   = _bridge x     <> _bridge y
                        , _ip       = _ip x         <> _ip y
                        }

