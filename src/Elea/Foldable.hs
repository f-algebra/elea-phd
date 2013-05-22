module Elea.Foldable
(
  module Data.Functor.Foldable,
  Refoldable, FoldableM (..),
  transformM, rewriteM, foldM, 
  allM, findM, anyM, any, all,
  transform, rewrite, recover,
  rewriteStepsM, rewriteSteps,
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable, allM, anyM, findM, any, all )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable
import qualified Data.Monoid as Monoid

type Refoldable t = (Foldable t, Unfoldable t)

-- | Monadic cata- and para-morphisms for a provided monadic constraint.
class Refoldable t => FoldableM t where
  type FoldM t m :: Constraint 
  type FoldM t m = ()
 
  distM :: (Monad m, FoldM t m) => Base t (m a, t) -> m (Base t a)
  
  cataM :: forall m a . (Monad m, FoldM t m) => 
    (Base t a -> m a) -> t -> m a
  cataM f = join . liftM f . distM . fmap (cataM f &&& id) . project
    
  paraM :: forall m a . (Monad m, FoldM t m) =>
    (Base t (a, t) -> m a) -> t -> m a
  paraM f = liftM fst . cataM g
    where
    g :: Base t (a, t) -> m (a, t)
    g x = do
      a <- f x
      return (a, recover x)
  
transformM :: (FoldableM t, Monad m, FoldM t m) => 
  (t -> m t) -> t -> m t
transformM f = cataM (f . embed)
  
foldM :: (FoldableM t, Monad m, FoldM t (WriterT w m), Monoid w) =>
  (t -> m w) -> t -> m w
foldM f = execWriterT . rewriteM rrwt
  where
  rrwt = const (return Nothing) <=< tell <=< (lift . f)
  
findM :: (FoldableM t, Monad m, FoldM t (WriterT (Monoid.First a) m)) =>
  (t -> m (Maybe a)) -> t -> m (Maybe a)
findM f = liftM (Monoid.getFirst) . foldM (liftM Monoid.First . f)

allM :: (FoldableM t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
allM p = liftM Monoid.getAll . foldM (liftM Monoid.All . p)

anyM :: (FoldableM t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
anyM p = liftM not . allM (liftM not . p)

all :: (FoldableM t, FoldM t (WriterT Monoid.All Identity)) => 
  (t -> Bool) -> t -> Bool
all p = runIdentity . allM (return . p)

any :: (FoldableM t, FoldM t (WriterT Monoid.All Identity)) => 
  (t -> Bool) -> t -> Bool
any p = not . all (not . p)
  
rewriteM :: (FoldableM t, Monad m, FoldM t m) =>
  (t -> m (Maybe t)) -> t -> m t
rewriteM f = transformM rrwt
  where
  rrwt t = join . liftM (maybe (return t) (rewriteM f)) . f $ t

rewriteStepsM :: (FoldableM t, Monad m, FoldM t m) =>
  [t -> m (Maybe t)] -> t -> m t
rewriteStepsM steps = rewriteM (\t -> firstM (map ($ t) steps))
      
recover :: Unfoldable t => Base t (a, t) -> t
recover = embed . fmap snd
    
transform :: Refoldable t => (t -> t) -> t -> t
transform f = cata (f . embed)

rewrite :: Refoldable t => (t -> Maybe t) -> t -> t
rewrite f = transform $ \t -> maybe t (rewrite f) (f t)

rewriteSteps :: Refoldable t => [t -> Maybe t] -> t -> t
rewriteSteps steps = rewrite step
  where
  step t = Monoid.getFirst (concatMap (Monoid.First . ($ t)) steps)
