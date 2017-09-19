{-# LANGUAGE RankNTypes             #-}

-- |
-- Module: Sgf.Data.Generics.Aliases
--
-- Recursive generic queries.

module Sgf.Data.Generics.Aliases
    (
    -- * Definition.
    --
    -- $generic
      GenericRecQ (..)
    , foreverQ
    , extRecQ
    , extRecQ'
    , mkRecQ

    -- * Combinators for use with 'everythingBut' style schemes.
    --
    -- $genericBut
    , RecB
    , extRecL
    , extRecS
    , mkRecL
    , mkRecS
    , endGQ
    , end
    , end'
    , endQ
    , pureQ
    , stop
    , next
    )
  where

import Data.Generics
import Control.Arrow


-- $generic

-- | Recursive 'GenericQ' query, which returns new query at each step.
newtype GenericRecQ r   = RQ {unRQ :: GenericQ (r, GenericRecQ r)}

instance Functor GenericRecQ where
    fmap f (RQ g)   = RQ $ (f *** fmap f) <$> g

-- | Final query for 'GenericRecQ' recursive query chain. It will loop
-- forever.
foreverQ :: GenericQ r -> GenericRecQ r
foreverQ f          = RQ $ \x -> (f x, foreverQ f)

-- | Operator for building recursive query chains.
--
-- >    extRecQ g cont k
--
-- will call function @k@ and return @cont@ as the next 'GenericRecQ' query,
-- if argument type matches. Otherwise it will call default query @g@ and
-- /rebuild/ itself as the next query.  Versions with predefined default query
-- may be used to build chains of queries, which should be called one after
-- another (see 'extRecL' and 'extRecS').
extRecQ :: Typeable b => GenericQ r -> GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ g cont k    = RQ $      (\x -> (g x, extRecQ g cont k))
                        `extQ`  (\x -> (k x, cont))

-- | Monoid version of 'extRecQ'.
extRecQ' :: (Monoid r, Typeable b) => GenericRecQ r -> (b -> r) -> GenericRecQ r
extRecQ'            = extRecQ (const mempty)

-- | Make recursive query.
--
-- >    mkRecQ def k
--
-- will call function @k@ and expect it to return next 'GenericRecQ' query as
-- well, if argument type matches.  Otherwise, it will call default query @g@
-- and /rebuild/ itself as the next query.  It may be used, when function @g@
-- needs to decide, which next 'GenericRecQ' query to use, depending on its
-- argument.
mkRecQ :: Typeable b => GenericQ r -> (b -> (r, GenericRecQ r)) -> GenericRecQ r
mkRecQ g k          = RQ $ (\x -> (g x, mkRecQ g k)) `extQ` k


-- $genericBut

-- | Type of recursive query used with 'Sgf.Data.Generics.Schemes.everythingRecBut'.
type RecB r         = ((r, Bool), GenericRecQ (r, Bool))

-- | Loose 'extRecQ' binding: default function just skips non-matching types.
extRecL :: (Monoid r, Typeable b) =>
           GenericRecQ (r, Bool) -> (b -> (r, Bool)) -> GenericRecQ (r, Bool)
extRecL             = extRecQ (const (mempty, False))

-- | Tight (strict) 'extRecQ' binding: default function stops traversal on
-- non-matching types.
extRecS :: (Monoid r, Typeable b) =>
           GenericRecQ (r, Bool) -> (b -> (r, Bool)) -> GenericRecQ (r, Bool)
extRecS             = extRecQ (const (mempty, True))

-- | Make a loose 'GenericRecQ' query, which may match somewhere further down
-- the data constructor tree.
mkRecL :: (Monoid r, Typeable b) =>
          (b -> ((r, Bool), GenericRecQ (r, Bool))) -> GenericRecQ (r, Bool)
mkRecL              = mkRecQ (const (mempty, False))

-- | Make a tight (strict) 'GenericRecQ' query, which should match, when it's
-- called.
mkRecS :: (Monoid r, Typeable b) =>
          (b -> ((r, Bool), GenericRecQ (r, Bool))) -> GenericRecQ (r, Bool)
mkRecS              = mkRecQ (const (mempty, True))

-- | Final 'GenericRecQ' query, which prevents further data traversal.
endGQ :: GenericQ r -> GenericRecQ (r, Bool)
endGQ f             = foreverQ (\x -> (f x, True))

-- | Make a final 'GenericRecQ' query from a /non-generic/ function.
endQ :: (Typeable b, Monoid r) => (b -> r) -> GenericRecQ (r, Bool)
endQ f              = endGQ (mempty `mkQ` f)

-- | Final value, which will prevent further data traversal in schemes like
-- 'everythingRecBut'.
end :: r -> GenericRecQ (r, Bool)
end d               = foreverQ (const (d, True))

-- | 'Monoid' version of 'end'.
end' :: Monoid r => GenericRecQ (r, Bool)
end'                = foreverQ (const (mempty, True))

-- | Lift predicate into identity query determining whether to traverse
-- constructors tree deeper.
pureQ :: Monoid r => (a -> Bool) -> a -> (r, Bool)
-- Note, that 'everythingBut' schemes expect 'False' for /continuing/
-- traversal and 'True' to /stop/ it (thus, predicate value is reversed).
pureQ p x           = (mempty, not (p x))

-- | Stop constructors tree traversal.
stop :: r -> ((r, Bool), GenericRecQ (r, Bool))
stop d              = ((d, True), undefined)

-- | Use supplied 'GenericRecQ' query as next query.
next :: Monoid r => GenericRecQ (r, Bool) -> ((r, Bool), GenericRecQ (r, Bool))
-- Note, that returning 'True' has no sense, because then supplied query will
-- never be called.
next cont           = ((mempty, False), cont)

