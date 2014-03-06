module Elea.Foldable
(
  module Data.Functor.Foldable,
  Bifoldable, FoldableM (..), TransformableM (..), 
  Iso, iso, 
  isoTransformM, isoRewriteM, isoRewriteStepsM,
  isoFindM, isoAnyM, isoAllM, isoRewriteOnceM, isoFoldM,
  isoRewrite, isoTransform, isoFind, isoRewriteM',
  rewriteM, foldM, rewriteOnceM, collectM, isoCollectM,
  allM, findM, anyM, any, all, isoFold, isoAny, isoAll, find,
  collect, count,
  transform, rewrite, recover,
  rewriteStepsM, rewriteSteps, countM,
  SelectorM, selectiveTransformM, selectAll,
)
where

import Prelude ()
import Elea.Prelude hiding ( Foldable, allM, anyM, findM, any, all, find, )
import GHC.Prim ( Constraint )
import Data.Functor.Foldable
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Failure as Fail
import qualified Data.Monoid as Monoid
import qualified Control.Monad.State as State
import qualified Data.Isomorphism as Iso
import qualified Data.Set as Set

type Bifoldable t = (Foldable t, Unfoldable t)

-- | Monadic cata- and para-morphisms for a provided monadic constraint.
class (Monad m, Bifoldable t) => FoldableM m t where

  -- We can implement all of these transformations purely in terms
  -- of this distributivity law. Sexy.
  distM :: Base t (m a, t) -> m (Base t a)
   
  cataM :: forall a . (Base t a -> m a) -> t -> m a
  cataM f = 
    join . liftM f . distM . fmap (cataM f &&& id) . project
    
  paraM :: forall a . (Base t (a, t) -> m a) -> t -> m a
  paraM f = liftM fst . cataM g
    where
    g :: Base t (a, t) -> m (a, t)
    g x = do
      a <- f x
      return (a, recover x)
      
recover :: Unfoldable t => Base t (a, t) -> t
recover = embed . fmap snd
     
class FoldableM m t => TransformableM m t where
  transformM :: (t -> m t) -> t -> m t
  transformM f = cataM (f . embed)
 
-- | I was fed up of having to rewrite the 'transform', 'rewrite', 'any', etc.
-- functions for isomorphisms of an existing datatype.
type Iso = Iso.Iso (->) 

iso :: (a -> b) -> (b -> a) -> Iso a b
iso = Iso.Iso

isoTransformM :: TransformableM m t =>
  Iso a t -> (a -> m a) -> a -> m a
isoTransformM iso f = id
  . liftM (Iso.project iso)
  . transformM (liftM (Iso.embed iso) . f . Iso.project iso) 
  . Iso.embed iso
  
isoRewriteM :: TransformableM m t =>
  Iso a t -> (a -> MaybeT m a) -> a -> m a
isoRewriteM iso f = id
  . liftM (Iso.project iso)
  . transformM rrwt 
  . Iso.embed iso
  where
  f' = liftM (Iso.embed iso) . f . Iso.project iso
  rrwt t = maybeT (return t) (rewriteM f') (f' t)
  
-- | A version of 'isoRewriteM' which checks whether any rewrites 
-- were actually performed; returning 'Nothing' if none were.
isoRewriteM' :: forall a t m . 
    (Monad m, TransformableM (WriterT Monoid.Any m) t) =>
  Iso a t -> (a -> MaybeT m a) -> a -> MaybeT m a
isoRewriteM' iso f x = do
  (x', any) <- lift (runWriterT (isoRewriteM iso f' x))
  guard (Monoid.getAny any)
  return x'
  where
  f' :: a -> MaybeT (WriterT Monoid.Any m) a
  f' x = do
    x' <- MaybeT (lift (runMaybeT (f x)))
    tell (Monoid.Any True)
    return x'
    
rewriteM :: TransformableM m t =>
  (t -> MaybeT m t) -> t -> m t
rewriteM = isoRewriteM id

type WriterTransformableM w m t = 
  (Monad m, Monoid w, TransformableM (WriterT w m) t)

foldM :: WriterTransformableM w m t => (t -> m w) -> t -> m w
foldM = isoFoldM id

findM :: WriterTransformableM (Monoid.First b) m t => 
  (t -> m (Maybe b)) -> t -> m (Maybe b)
findM = isoFindM id

allM, anyM :: WriterTransformableM Monoid.All m t => 
  (t -> m Bool) -> t -> m Bool
allM = isoAllM id
anyM = isoAnyM id

collectM :: (Ord b, WriterTransformableM (Set b) m t) =>
  (t -> MaybeT m b) -> t -> m (Set b)
collectM = isoCollectM id

collect :: (Ord b, WriterTransformableM (Set b) Identity t) =>
  (t -> Maybe b) -> t -> Set b
collect f = runIdentity . collectM (MaybeT . Identity . f)

all, any :: WriterTransformableM Monoid.All Identity t =>
  (t -> Bool) -> t -> Bool
all p = runIdentity . allM (return . p)
any p = not . all (not . p)

find :: WriterTransformableM (Monoid.First b) Identity t => 
  (t -> Maybe b) -> t -> Maybe b
find = isoFind id

isoFoldM :: forall w m t a . WriterTransformableM w m t =>
  Iso a t -> (a -> m w) -> a -> m w
isoFoldM iso f = execWriterT . isoRewriteM iso tellAll
  where
  tellAll :: a -> MaybeT (WriterT w m) a 
  tellAll t = do
    w <- (lift . lift . f) t
    tell w
    mzero
    
isoFindM :: WriterTransformableM (Monoid.First b) m t =>
  Iso a t -> (a -> m (Maybe b)) -> a -> m (Maybe b)
isoFindM iso f = id
  . liftM (Monoid.getFirst) 
  . isoFoldM iso (liftM Monoid.First . f)
  
isoAllM, isoAnyM :: WriterTransformableM Monoid.All m t =>
  Iso a t -> (a -> m Bool) -> a -> m Bool
isoAllM iso p = liftM Monoid.getAll . isoFoldM iso (liftM Monoid.All . p)
isoAnyM iso p = liftM not . isoAllM iso (liftM not . p)

isoFold :: WriterTransformableM w Identity t =>
  Iso a t -> (a -> w) -> a -> w
isoFold iso f = runIdentity . isoFoldM iso (Identity . f)

isoCollectM :: (Ord b, WriterTransformableM (Set b) m t) =>
  Iso a t -> (a -> MaybeT m b) -> a -> m (Set b)
isoCollectM iso f = 
  isoFoldM iso (liftM (maybe mempty Set.singleton) . runMaybeT . f)

countM :: WriterTransformableM (Monoid.Sum Int) m t => 
  (t -> m Bool) -> t -> m Int
countM p = liftM Monoid.getSum . foldM (liftM (Monoid.Sum . found) . p)
  where
  found :: Bool -> Int
  found False = 0
  found True = 1
  
count :: WriterTransformableM (Monoid.Sum Int) Identity t => 
  (t -> Bool) -> t -> Int
count p = runIdentity . countM (Identity . p)

-- | Apply a given transformation exactly once. If it is never applied
-- then this returns 'Nothing'.
isoRewriteOnceM :: forall a m t .
    (Monad m, TransformableM (StateT Bool m) t) =>
  Iso a t -> (a -> MaybeT m a) -> a -> MaybeT m a
isoRewriteOnceM iso f t = do
  (t', done) <- lift (runStateT (isoTransformM iso once t) False)
  guard done
  return t'
  where
  once :: a -> StateT Bool m a
  once t = do
    already <- State.get
    if already 
    then return t
    else do
      mby_t <- lift (runMaybeT (f t))
      case mby_t of
        Nothing -> return t
        Just t' -> do
          State.put True
          return t'
          
rewriteOnceM :: (Monad m, TransformableM (StateT Bool m) t) =>
  (t -> MaybeT m t) -> t -> MaybeT m t
rewriteOnceM = isoRewriteOnceM id

isoRewriteStepsM :: (Monad m, TransformableM m t) =>
  Iso a t -> [a -> MaybeT m a] -> a -> m a
isoRewriteStepsM iso steps = 
  isoRewriteM iso rrwt
  where
  rrwt t = MaybeT $ firstM (map (\f -> runMaybeT (f t)) steps)
   
rewriteStepsM :: (Monad m, TransformableM m t) =>
  [t -> MaybeT m t] -> t -> m t
rewriteStepsM = isoRewriteStepsM id
    
isoTransform :: Bifoldable t => Iso a t -> (a -> a) -> a -> a
isoTransform iso f = id
  . Iso.project iso
  . cata (Iso.embed iso . f . Iso.project iso . embed)
  . Iso.embed iso

isoRewrite :: Bifoldable t => Iso a t -> (a -> Maybe a) -> a -> a
isoRewrite iso f = Iso.project iso . transform rrwt . Iso.embed iso
  where
  f' = fmap (Iso.embed iso) . f . Iso.project iso
  rrwt t = maybe t (rewrite f') (f' t)
  
isoFind :: WriterTransformableM (Monoid.First b) Identity t =>
  Iso a t -> (a -> Maybe b) -> a -> Maybe b
isoFind iso f = runIdentity . isoFindM iso (return . f) 

isoAny, isoAll :: WriterTransformableM Monoid.All Identity t =>
  Iso a t -> (a -> Bool) -> a -> Bool
isoAll iso p = runIdentity . isoAllM iso (return . p)
isoAny iso p = not . isoAll iso (not . p)
  
transform :: Bifoldable t => (t -> t) -> t -> t
transform = isoTransform id

rewrite :: Bifoldable t => (t -> Maybe t) -> t -> t
rewrite = isoRewrite id

rewriteSteps :: Bifoldable t => [t -> Maybe t] -> t -> t
rewriteSteps steps = rewrite step
  where
  step t = Monoid.getFirst (concatMap (Monoid.First . ($ t)) steps)


-- | This "selector" restricts how you transform something recursively. 
-- The outermost boolean tells you when to apply the transformation. 
-- The inner boolean tells you when to recurse.
type SelectorM m t = t -> m (Bool, Base t Bool)

selectAll :: (Foldable t, Monad m) => SelectorM m t
selectAll = return 
  . (\t -> (True, t))
  . fmap (\_ -> True) 
  . project

selectiveTransformM :: forall t m . (FoldableM m t, Zip (Base t)) => 
  SelectorM m t -> (t -> m t) -> t -> m t
selectiveTransformM p f t = do
  (here, desc :: Base t Bool) <- p t
  let desc' :: Base t (Bool, t)
      desc' = zip desc (project t)
  t' <- id
    . liftM embed 
    . distM
    $ fmap descent desc'
  if here
  then f t'
  else return t'
  where
  descent :: (Bool, t) -> (m t, t)
  descent (True, t) = (selectiveTransformM p f t, t)
  descent (False, t) = (return t, t)

