module Elea.Foldable
(
  module Data.Functor.Foldable,
  Refoldable, FoldableM (..),
  rewriteM, transform, rewrite, recover
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable

type Refoldable t = (Foldable t, Unfoldable t)

-- | Monadic cata- and para-morphisms for a provided monadic constraint.
class Refoldable t => FoldableM t where
  type FoldM t m :: Constraint 
  type FoldM t m = ()
  
  -- This is how I should be implementing cataM I think, but I figured
  -- this out later, now I can't be bothered to rewrite everything, yet.
  -- invertM :: (Monad m, FoldM t m) => Base t (m a, t) -> m (Base t a)
  
  cataM :: (Monad m, FoldM t m) => 
    (Base t a -> m a) -> t -> m a
  -- cataM f = join . liftM f . invertM . fmap (cataM f &&& id) . project
    
  paraM :: forall m a . (Monad m, FoldM t m) =>
    (Base t (a, t) -> m a) -> t -> m a
  paraM f = liftM fst . cataM g
    where
    g :: Base t (a, t) -> m (a, t)
    g x = do
      a <- f x
      return (a, recover x)
  
  transformM :: (Monad m, FoldM t m) => 
    (t -> m t) -> t -> m t
  transformM f = cataM (f . embed)
  
rewriteM :: (FoldableM t, Monad m, FoldM t m) =>
  (t -> m (Maybe t)) -> t -> m t
rewriteM f = 
  transformM $ \t -> 
    join . liftM (maybe (return t) (rewriteM f)) $ f t
      
recover :: Unfoldable t => Base t (a, t) -> t
recover = embed . fmap snd
    
transform :: Refoldable t => (t -> t) -> t -> t
transform f = cata (f . embed)

rewrite :: Refoldable t => (t -> Maybe t) -> t -> t
rewrite f = transform $ \t -> maybe t (rewrite f) (f t)
