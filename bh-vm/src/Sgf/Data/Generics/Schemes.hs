{-# LANGUAGE RankNTypes             #-}

module Sgf.Data.Generics.Schemes
    ( everythingR
    , everythingRec
    , everythingRecBut
    )
  where

import Data.Generics
import Control.Monad.Reader

import Sgf.Data.Generics.Aliases


-- | 'everything' traversal in context of 'Reader' updated at each element
-- with given function.
everythingR :: (MonadReader r m, Data a) => GenericQ (r -> r)
               -> (m t -> m t -> m t) -> GenericQ (m t) -> a -> m t
everythingR f k q x = local (f x)
                        $ foldl k (q x) (gmapQ (everythingR f k q) x)

-- | 'everything' traversal for recursive query 'GenericRecQ'. The new query
-- is applied to all subterms at one level.
everythingRec :: Data a => (r -> r -> r) -> GenericRecQ r -> a -> r
everythingRec k q x     =
    let (z, q') = unRQ q x
    in  foldl k z (gmapQ (everythingRec k q') x)

-- | 'everythingBut' traversal for recursive query 'GenericRecQ'. The new
-- query is applied to all subterms at one level.
everythingRecBut :: Data a => (r -> r -> r) -> GenericRecQ (r, Bool) -> a -> r
everythingRecBut k q x  =
    let ((z, b), q') = unRQ q x
    in  if b then z else foldl k z (gmapQ (everythingRecBut k q') x)

