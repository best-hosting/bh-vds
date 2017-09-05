
-- |
-- Module: Internal.Data.Generics.Lenses
--
-- Lenses implementation using generics. Note, that there will be /no/ error,
-- if a type does /not/ contain a record with required type.

module Internal.Data.Generics.Lenses
  where

import Data.Generics


-- | Set a record with matched type to specified value.
set :: (Typeable b, Data a) => b -> a -> a
set y               = gmapT (mkT (const y))

-- | Modify a record with matched type.
modify :: (Typeable b, Data a) => (b -> b) -> a -> a
modify f            = gmapT (mkT f)

-- | Modify a record with matched type using monadic function.
modifyM :: (Typeable b, Data a, Monad m) => (b -> m b) -> a -> m a
modifyM f           = gmapM (mkM f)

