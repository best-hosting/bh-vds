{-# LANGUAGE RankNTypes             #-}

module Sgf.Data.Generics.Aliases
    ( GenericRecQ (..)
    , foreverQ
    , endQ
    , end
    , end'
    , lastQ
    , lastQ2
    , extRecQ
    , mkRecQ
    , extRecQ'
    , extRecL
    , mkRecL
    , extRecS
    , mkRecS
    )
  where

import Data.Generics
import Control.Arrow


-- | Recursive 'GenericQ' query, which updates itself at each step.
newtype GenericRecQ r   = RQ {unRQ :: GenericQ (r, GenericRecQ r)}

instance Functor GenericRecQ where
    fmap f (RQ g)   = RQ $ (f *** fmap f) <$> g

-- | Final query for 'GenericRecQ' recursive query chain. It will loop
-- forever.
foreverQ :: GenericQ r -> GenericRecQ r
foreverQ f          = RQ $ \x -> (f x, foreverQ f)

-- | Final query, which will prevent further data traversal in schemes like
-- 'everythingRecBut'.
endQ :: GenericQ r -> GenericRecQ (r, Bool)
endQ f              = foreverQ (\x -> (f x, True))

-- | Final value, which will prevent further data traversal in schemes like
-- 'everythingRecBut'.
end :: r -> GenericRecQ (r, Bool)
end d               = foreverQ (const (d, True))

-- | 'Monoid' version of 'end'.
end' :: Monoid r => GenericRecQ (r, Bool)
end'                = foreverQ (const (mempty, True))

-- | Make the function (not generic query) preventing further data traversal
-- in schemes like 'everythingRecBut'.
lastQ :: (b -> r) -> b -> (r, Bool)
lastQ f x           = (f x, True)

lastQ2 :: (Typeable b, Monoid r) => (b -> r) -> GenericRecQ (r, Bool)
lastQ2 f             = mkRecS $ \x -> ((f x, True), undefined)

-- | Operator for building recursive query chains.
--
--      extRecQ g cont k
--
-- will call function @k@ and return @cont@ as the next 'GenericRecQ' query,
-- if argument type matches, otherwise it will call default query @g@.
extRecQ :: Typeable b => GenericQ r -> GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ g cont k    = RQ $      (\x -> (g x, extRecQ g cont k))
                        `extQ`  (\x -> (k x, cont))

-- | This version of `extRecQ` allows to choose different continuations (next
-- queries) depending on value of type 'b'. It's in fact a `mkRecQ`.
mkRecQ :: Typeable b => GenericQ r -> (b -> (r, GenericRecQ r)) -> GenericRecQ r
mkRecQ g k          = RQ $ (\x -> (g x, mkRecQ g k)) `extQ` k
-- | Monoid version of 'extRecQ'.
extRecQ' :: (Monoid r, Typeable b) => GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ'            = extRecQ (const mempty)

-- | Loose 'extRecQ' binding: default function just skips non-matching types.
-- For use with traversal schemes like 'everythingRecBut'.
extRecL :: (Monoid r, Typeable b) => GenericRecQ (r, Bool) -> (b -> (r, Bool)) -> GenericRecQ (r, Bool)
extRecL             = extRecQ (const (mempty, False))

mkRecL :: (Monoid r, Typeable b) => (b -> ((r, Bool), GenericRecQ (r, Bool))) -> GenericRecQ (r, Bool)
mkRecL              = mkRecQ (const (mempty, False))

-- | Tight 'extRecQ' binding: default function aborts traversal on
-- non-matching types. For use with traversal schemes like 'everythingRecBut'.
extRecS :: (Monoid r, Typeable b) => GenericRecQ (r, Bool) -> (b -> (r, Bool)) -> GenericRecQ (r, Bool)
extRecS             = extRecQ (const (mempty, True))

mkRecS :: (Monoid r, Typeable b) => (b -> ((r, Bool), GenericRecQ (r, Bool))) -> GenericRecQ (r, Bool)
mkRecS              = mkRecQ (const (mempty, True))

