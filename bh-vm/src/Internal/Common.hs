
module Internal.Common
  where

import Data.Monoid
import Data.Maybe
import Data.Yaml.Aeson
import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import qualified Filesystem.Path.CurrentOS as F


-- $tuples
-- * Operatins over tuples.

-- | 'mappend' over first tuple argument, '||' over second.
infixr 3 *|
(*|) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*|)                = uncurry (***) . (mappend *** (||))

-- | 'mappend' over first tuple argument, '&&' over second.
infixr 3 *&
(*&) :: Monoid r => (r, Bool) -> (r, Bool) -> (r, Bool)
(*&)                = uncurry (***) . (mappend *** (&&))

-- $lift
-- * Lift operations.
infixr 4 <&&>
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>)              = liftA2 (&&)

infixr 4 <*|>
(<*|>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*|>)              = liftA2 (*|)

infixr 4 <*&>
(<*&>) :: (Applicative f, Monoid r) =>
          f (r, Bool) -> f (r, Bool) -> f (r, Bool)
(<*&>)              = liftA2 (*&)

fromLast :: Monoid a => Last a -> a
fromLast            = fromMaybe mempty . getLast

fromFirst :: Monoid a => First a -> a
fromFirst           = fromMaybe mempty . getFirst

decodeFileEitherF :: (MonadIO m, FromJSON a) =>
                     F.FilePath -> m (Either (F.FilePath, ParseException) a)
decodeFileEitherF f = either (\e -> Left (f, e)) Right <$>
                       (liftIO . decodeFileEither . F.encodeString $ f)

toLast :: a -> Last a
toLast              = Last . Just

toFirst :: a -> First a
toFirst             = First . Just

notEmpty :: (Eq a, Monoid a) => a -> Bool
notEmpty            = not . (mempty ==)

