
-- |
-- Module: Internal.Common
--
-- Several common functions.

module Internal.Common
  where

import           Data.Monoid
import           Data.Maybe
import           Data.Yaml.Aeson
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Exception
import           Control.Monad.Managed
import qualified Filesystem.Path.CurrentOS  as F
import           System.Directory (getTemporaryDirectory)
import           Turtle (Text, writeTextFile, mktempfile)

import           System.Libvirt.Types


-- * Operatins over tuples.

-- | 'mappend' over first tuple argument, '||' over second.
infixr 3 *|
(*|) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*|)                = uncurry (***) . (mappend *** (||))

-- | 'mappend' over first tuple argument, '&&' over second.
infixr 3 *&
(*&) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*&)                = uncurry (***) . (mappend *** (&&))

-- * Lift operations.
-- | Lift '&&' into 'Applicative'.
infixr 4 <&&>
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>)              = liftA2 (&&)

-- | Lift '*|' into 'Applicative'.
infixr 4 <*|>
(<*|>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*|>)              = liftA2 (*|)

-- | Lift '*&' into 'Applicative'.
infixr 4 <*&>
(<*&>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*&>)              = liftA2 (*&)

-- | Unwrap 'Last' for 'Monoid'-s.
fromLast :: Monoid a => Last a -> a
fromLast            = fromMaybe mempty . getLast

-- | Unwrap 'First' for 'Monoid'.
fromFirst :: Monoid a => First a -> a
fromFirst           = fromMaybe mempty . getFirst

-- | Wrap into 'Last'.
toLast :: a -> Last a
toLast              = Last . Just

-- | Wrap into 'First'.
toFirst :: a -> First a
toFirst             = First . Just

-- | Check, that a value is not 'mempty'.
notEmpty :: (Eq a, Monoid a) => a -> Bool
notEmpty            = (mempty /=)

-- | Add filepath to exception returned by 'decodeFileEither'.
decodeFileEither' :: (MonadIO m, FromJSON a) => F.FilePath -> m a
decodeFileEither' f = do
    r <- liftIO . decodeFileEither . F.encodeString $ f
    either (throw . YamlParseError f) return r

-- | Create temporary file in a safe way (in 'MonadManaged'), write 'Text' to
-- it and return its filename.
writeTempFile :: MonadManaged m => Text -> Text -> m F.FilePath
writeTempFile n c   = do
    tmp <- liftIO getTemporaryDirectory
    tf  <- mktempfile (F.decodeString tmp) n
    liftIO $ writeTextFile tf c
    return tf

