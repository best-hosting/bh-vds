{-# LANGUAGE Rank2Types #-}

-- |
-- Module: Internal.Control.Lens
--
-- The simple lens implementation.

module Internal.Control.Lens
    ( LensA
    , viewA
    , viewAmaybe
    , setA
    , modifyA
    , modifyAA
    , maybeL
    , nothingL
    , headL
    )
  where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Identity

-- | Redefine lenses.
type LensA a b      = forall f. Applicative f => (b -> f b) -> a -> f a

-- | View a record value. Will /fail/, if constructor does /not/ have required
-- record.
viewA :: LensA a b -> a -> b
viewA l             = fromJust . getLast . getConst . l (Const . Last . Just)
-- | View a 'Maybe' record value. Will /not/ fail if constructor does not have
-- required record.
viewAmaybe :: LensA a b -> a -> Maybe b
viewAmaybe l        = getLast . getConst . l (Const . Last . Just)

-- | Modify record.
modifyA :: LensA a b -> (b -> b) -> a -> a
modifyA l f         = runIdentity . l (Identity . f)

-- | Modify record using a function in 'Applicative'.
modifyAA :: Applicative t => LensA a b -> (b -> t b) -> a -> t a
modifyAA l f        = runIdentityT . l (IdentityT . f)

-- | Set a record.
setA :: LensA a b -> b -> a -> a
setA l s            = modifyA l (const s)

-- | Lens to value in Maybe. If there is Nothing, original value in Functor
-- returned. Thus, i need Applicative.
maybeL :: LensA (Maybe a) a
maybeL f x          = maybe (pure x) (fmap Just . f) x

-- | Lens from some type to Maybe, which always sees Nothing and ignores
-- changes of Maybe record.  It may be used for value, which does not have
-- (Maybe a) record at all.
nothingL :: LensA a (Maybe b)
nothingL f x        = fmap (const x) (f Nothing)

-- | Lens to list head.
headL :: LensA [a] a
headL f []          = pure []
headL f (x : xs)    = (: xs) <$> f x

