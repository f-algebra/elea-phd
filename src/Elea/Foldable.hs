module Elea.Foldable
(
  module Data.Functor.Foldable,
  Refoldable, FoldableM (..), Transformable (..),
  rewriteM, foldM, rewriteOnceM, collectM,
  allM, findM, anyM, any, all,
  transform, rewrite, recover,
  rewriteStepsM, rewriteSteps, countM,
  SelectorM, selectiveTransformM, 
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable, allM, anyM, findM, any, all, zip )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable
import qualified Elea.Prelude as Prelude
import qualified Data.Monoid as Monoid
import qualified Control.Monad.State as State
import qualified Data.Set as Set

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
  
class FoldableM t => Transformable t where
  transformM :: (Monad m, FoldM t m) => 
    (t -> m t) -> t -> m t
  transformM f = cataM (f . embed)
  
foldM :: (Transformable t, Monad m, FoldM t (WriterT w m), Monoid w) =>
  (t -> m w) -> t -> m w
foldM f = execWriterT . rewriteM rrwt
  where
  rrwt = const (return Nothing) <=< tell <=< (lift . f)
  
findM :: (Transformable t, Monad m, FoldM t (WriterT (Monoid.First a) m)) =>
  (t -> m (Maybe a)) -> t -> m (Maybe a)
findM f = liftM (Monoid.getFirst) . foldM (liftM Monoid.First . f)

allM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
allM p = liftM Monoid.getAll . foldM (liftM Monoid.All . p)

anyM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
anyM p = liftM not . allM (liftM not . p)

collectM :: (Ord a, Transformable t, Monad m, FoldM t (WriterT (Set a) m)) =>
  (t -> MaybeT m a) -> t -> m (Set a)
collectM f = foldM (liftM (maybe mempty Set.singleton) . runMaybeT . f)

countM :: (Transformable t, Monad m, FoldM t (WriterT (Monoid.Sum Int) m)) => 
  (t -> m Bool) -> t -> m Int
countM p = liftM Monoid.getSum . foldM (liftM (Monoid.Sum . found) . p)
  where
  found :: Bool -> Int
  found False = 0
  found True = 1

all :: (Transformable t, FoldM t (WriterT Monoid.All Identity)) => 
  (t -> Bool) -> t -> Bool
all p = runIdentity . allM (return . p)

any :: (Transformable t, FoldM t (WriterT Monoid.All Identity)) => 
  (t -> Bool) -> t -> Bool
any p = not . all (not . p)

-- | Apply a given transformation exactly once. If it is never applied
-- then this returns 'Nothing'.
rewriteOnceM :: forall m t .
    (Transformable t, Monad m, FoldM t (StateT Bool m)) =>
  (t -> m (Maybe t)) -> t -> m (Maybe t)
rewriteOnceM f t = do
  (t', done) <- runStateT (transformM once t) False
  if done
  then return (Just t')
  else return Nothing
  where
  once :: Monad m => t -> StateT Bool m t
  once t = do
    already <- State.get
    if already 
    then return t
    else do
      mby_t <- lift (f t)
      case mby_t of
        Nothing -> return t
        Just t' -> do
          State.put True
          return t'
          
rewriteM :: (Transformable t, Monad m, FoldM t m) =>
  (t -> m (Maybe t)) -> t -> m t
rewriteM f = transformM rrwt
  where
  rrwt t = join . liftM (maybe (return t) (rewriteM f)) . f $ t

rewriteStepsM :: (Transformable t, Monad m, FoldM t m) =>
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

type SelectorM m t = t -> m (Bool, Base t (Bool, t))
  
selectiveTransformM :: forall t m . (FoldableM t, FoldM t m, Monad m) => 
  SelectorM m t -> (t -> m t) -> t -> m t
selectiveTransformM p f t = do
  (here, desc) <- p t
  t' <- id
    . liftM embed 
    . distM
    $ fmap descent desc
  if here
  then f t'
  else return t'
  where
  descent :: (Bool, t) -> (m t, t)
  descent (True, t) = (selectiveTransformM p f t, t)
  descent (False, t) = (return t, t)

