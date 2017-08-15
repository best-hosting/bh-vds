
module Sgf.Common
  where

import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Arrow


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

fromAlt :: Monoid a => Alt Maybe a -> a
fromAlt             = fromMaybe mempty . getAlt

