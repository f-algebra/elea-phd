module Elea.Foldable
(
  module Data.Functor.Foldable,
  Refoldable, FoldableM (..), UnfoldableM (..),
  rewriteM, transform, rewrite
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable

type Refoldable t = (Foldable t, Unfoldable t)

class Refoldable t => FoldableM t where
  type FoldM t m :: Constraint 
  
  foldM :: (Monad m, FoldM t m) => 
    (Base t a -> m a) -> t -> m a
  
  transformM :: (Monad m, FoldM t m) => 
    (t -> m t) -> t -> m t
  transformM f = foldM (f . embed)
  
class Refoldable t => UnfoldableM t where
  type UnfoldM t m :: Constraint
  
  unfoldM :: (Monad m, UnfoldM t m) =>
    (a -> m (Base t a)) -> a -> m t
  
rewriteM :: (FoldableM t, Monad m, FoldM t m) =>
  (t -> m (Maybe t)) -> t -> m t
rewriteM f = 
  transformM $ \t -> 
    join . liftM (maybe (return t) (rewriteM f)) $ f t
      
transform :: Refoldable t => (t -> t) -> t -> t
transform f = cata (f . embed)

rewrite :: Refoldable t => (t -> Maybe t) -> t -> t
rewrite f = transform $ \t -> maybe t (rewrite f) (f t)
