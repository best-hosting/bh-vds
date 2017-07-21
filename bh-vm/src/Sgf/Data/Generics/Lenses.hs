
module Sgf.Data.Generics.Lenses
  where

import Data.Generics


set :: (Typeable b, Data a) => b -> a -> a
set y               = gmapT (mkT (const y))

modify :: (Typeable b, Data a) => (b -> b) -> a -> a
modify f            = gmapT (mkT f)

