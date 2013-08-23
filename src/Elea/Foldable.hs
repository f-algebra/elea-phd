module Elea.Foldable
(
  module Data.Functor.Foldable,
  Refoldable, FoldableM (..), Transformable (..), 
  Iso, iso, 
  isoTransformM, isoRewriteM, isoRewriteStepsM,
  isoFindM, isoAnyM, isoAllM, isoRewriteOnceM,
  isoRewrite, isoTransform, isoFind, isoRewriteM',
  rewriteM, foldM, rewriteOnceM, collectM,
  allM, findM, anyM, any, all,
  transform, rewrite, recover,
  rewriteStepsM, rewriteSteps, countM,
  SelectorM, selectiveTransformM, selectAll,
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable, allM, anyM, findM, any, all, zip )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable
import qualified Elea.Prelude as Prelude
import qualified Data.Monoid as Monoid
import qualified Control.Monad.State as State
import qualified Data.Isomorphism as Iso
import qualified Data.Set as Set

type Refoldable t = (Foldable t, Unfoldable t)

-- | Monadic cata- and para-morphisms for a provided monadic constraint.
class Refoldable t => FoldableM t where
  type FoldM t m :: Constraint 
  type FoldM t m = ()
  
  -- We can implement all of these transformations purely in terms
  -- of this distributivity law. Sexy.
  distM :: (Monad m, FoldM t m) => Base t (m a, t) -> m (Base t a)
  
  {-# INLINEABLE cataM #-}
  cataM :: forall m a . (Monad m, FoldM t m) => 
    (Base t a -> m a) -> t -> m a
  cataM f = join . liftM f . distM . fmap (cataM f &&& id) . project

  {-# INLINEABLE paraM #-}
  paraM :: forall m a . (Monad m, FoldM t m) =>
    (Base t (a, t) -> m a) -> t -> m a
  paraM f = liftM fst . cataM g
    where
    g :: Base t (a, t) -> m (a, t)
    g x = do
      a <- f x
      return (a, recover x)
  
class FoldableM t => Transformable t where
  {-# INLINEABLE transformM #-}
  transformM :: (Monad m, FoldM t m) => 
    (t -> m t) -> t -> m t
  transformM f = cataM (f . embed)
 
-- | I was fed up of having to rewrite the 'transform', 'rewrite', 'any', etc.
-- functions for isomorphisms of an existing datatype.
type Iso = Iso.Iso (->) 

iso :: (a -> b) -> (b -> a) -> Iso a b
iso = Iso.Iso

isoTransformM :: (Transformable t, Monad m, FoldM t m) =>
  Iso a t -> (a -> m a) -> a -> m a
isoTransformM iso f = id
  . liftM (Iso.project iso)
  . transformM (liftM (Iso.embed iso) . f . Iso.project iso) 
  . Iso.embed iso
  
isoRewriteM :: (Transformable t, Monad m, FoldM t m) =>
  Iso a t -> (a -> m (Maybe a)) -> a -> m a
isoRewriteM iso f = id
  . liftM (Iso.project iso)
  . transformM rrwt 
  . Iso.embed iso
  where
  f' = liftM (fmap (Iso.embed iso)) . f . Iso.project iso
  rrwt t = join . liftM (maybe (return t) (rewriteM f')) . f' $ t
  
-- | A version of 'isoRewriteM' which checks whether any rewrites 
-- were actually performed; returning 'Nothing' if none were.
isoRewriteM' :: forall a t m .
    (Transformable t, Monad m, FoldM t (WriterT Monoid.Any m)) =>
  Iso a t -> (a -> m (Maybe a)) -> a -> m (Maybe a)
isoRewriteM' iso f x = do
  (x', any) <- runWriterT (isoRewriteM iso f' x)
  if Monoid.getAny any
  then return (Just x')
  else return Nothing
  where
  f' :: a -> WriterT Monoid.Any m (Maybe a)
  f' x = do
    mby_x' <- lift (f x)
    when (isJust mby_x') (tell (Monoid.Any True))
    return mby_x'
  
rewriteM :: (Transformable t, Monad m, FoldM t m) =>
  (t -> m (Maybe t)) -> t -> m t
rewriteM = isoRewriteM id

isoFoldM :: (Transformable t, Monad m, FoldM t (WriterT w m), Monoid w) =>
  Iso a t -> (a -> m w) -> a -> m w
isoFoldM iso f = execWriterT . isoRewriteM iso rrwt
  where
  rrwt = const (return Nothing) <=< tell <=< (lift . f)
  
isoFindM :: (Transformable t, Monad m, FoldM t (WriterT (Monoid.First f) m)) =>
  Iso a t -> (a -> m (Maybe f)) -> a -> m (Maybe f)
isoFindM iso f = id
  . liftM (Monoid.getFirst) 
  . isoFoldM iso (liftM Monoid.First . f)
  
isoAllM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  Iso a t -> (a -> m Bool) -> a -> m Bool
isoAllM iso p = liftM Monoid.getAll . isoFoldM iso (liftM Monoid.All . p)

isoAnyM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  Iso a t -> (a -> m Bool) -> a -> m Bool
isoAnyM iso p = liftM not . isoAllM iso (liftM not . p)

foldM :: (Transformable t, Monad m, FoldM t (WriterT w m), Monoid w) =>
  (t -> m w) -> t -> m w
foldM = isoFoldM id
  
findM :: (Transformable t, Monad m, FoldM t (WriterT (Monoid.First f) m)) =>
  (t -> m (Maybe f)) -> t -> m (Maybe f)
findM = isoFindM id

allM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
allM = isoAllM id

anyM :: (Transformable t, Monad m, FoldM t (WriterT Monoid.All m)) =>
  (t -> m Bool) -> t -> m Bool
anyM = isoAnyM id

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
isoRewriteOnceM :: forall a m t .
    (Transformable t, Monad m, FoldM t (StateT Bool m)) =>
  Iso a t -> (a -> m (Maybe a)) -> a -> m (Maybe a)
isoRewriteOnceM iso f t = do
  (t', done) <- runStateT (isoTransformM iso once t) False
  if done
  then return (Just t')
  else return Nothing
  where
  once :: Monad m => a -> StateT Bool m a
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
          
rewriteOnceM :: (Transformable t, Monad m, FoldM t (StateT Bool m)) =>
  (t -> m (Maybe t)) -> t -> m (Maybe t)
rewriteOnceM = isoRewriteOnceM id

isoRewriteStepsM :: (Transformable t, Monad m, FoldM t m) =>
  Iso a t -> [a -> m (Maybe a)] -> a -> m a
isoRewriteStepsM iso steps = isoRewriteM iso (\t -> firstM (map ($ t) steps))
   
rewriteStepsM :: (Transformable t, Monad m, FoldM t m) =>
  [t -> m (Maybe t)] -> t -> m t
rewriteStepsM = isoRewriteStepsM id

recover :: Unfoldable t => Base t (a, t) -> t
recover = embed . fmap snd
    
isoTransform :: Refoldable t => Iso a t -> (a -> a) -> a -> a
isoTransform iso f = id
  . Iso.project iso
  . cata (Iso.embed iso . f . Iso.project iso . embed)
  . Iso.embed iso

isoRewrite :: Refoldable t => Iso a t -> (a -> Maybe a) -> a -> a
isoRewrite iso f = Iso.project iso . transform rrwt . Iso.embed iso
  where
  f' = fmap (Iso.embed iso) . f . Iso.project iso
  rrwt t = maybe t (rewrite f') (f' t)
  
isoFind :: (Transformable t, FoldM t (Writer (Monoid.First f))) =>
  Iso a t -> (a -> Maybe f) -> a -> Maybe f
isoFind iso f = runIdentity . isoFindM iso (return . f) 
  
transform :: Refoldable t => (t -> t) -> t -> t
transform = isoTransform id

rewrite :: Refoldable t => (t -> Maybe t) -> t -> t
rewrite = isoRewrite id

rewriteSteps :: Refoldable t => [t -> Maybe t] -> t -> t
rewriteSteps steps = rewrite step
  where
  step t = Monoid.getFirst (concatMap (Monoid.First . ($ t)) steps)


-- | This "selector" restricts how you transform something recursively. 
-- The outermost boolean tells you when to apply the transformation. 
-- The inner boolean tells you when and where to recurse.
type SelectorM m t = t -> m (Bool, Base t (Bool, t))

selectAll :: (Foldable t, Monad m) => SelectorM m t
selectAll = return 
  . (\t -> (True, t))
  . fmap (\t -> (True, t)) 
  . project

{-# INLINEABLE selectiveTransformM #-}
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

