{-# LANGUAGE UndecidableInstances #-}
-- | Type environments, but also a lot of class instances for 'Term',
-- since they require type environments.
module Elea.Env 
(
  Writable (..), Readable (..),
  TrackIndices, TrackIndicesT (..),
  trackIndices, trackIndicesT,
  bind, bindMany,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Unifier ( Unifiable, Unifier )
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Index as Indices
import qualified Elea.Unifier as Unifier 
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Control.Monad.Trans as Trans

class Monad m => Writable m where
  bindAt :: Index -> Bind -> m a -> m a
  equals :: Term -> Term -> m a -> m a
  
bind :: Writable m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Writable m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

class Writable m => Readable m where
  bindings :: m [Bind]

  boundAt :: Index -> m Bind
  boundAt at = liftM (!! fromEnum at) bindings
  
  -- | Returns the number of indices that have been bound.
  bindingDepth :: m Int
  bindingDepth = liftM length bindings
  
instance Fold.FoldableM Term where
  type FoldM Term m = Writable m
  
  distM (Lam' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Lam' (Bind' l ty) t)
  distM (Fix' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Fix' (Bind' l ty) t)
  distM (Pi' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Pi' (Bind' l ty) t)
  distM (Ind' (Bind' l (mty, _ty)) mcs) = do
    ty <- mty
    cs <- bind (Bind l _ty) (mapM (sequence . map fst) mcs)
    return (Ind' (Bind' l ty) cs)
  distM (Case' (mt, _t) (mty, _ty) malts) = do
    alts <- zipWithM distAlt [0..] malts
    t <- mt
    ty <- mty
    return (Case' t ty alts)
    where
    distAlt n (Alt' mbs (mt, _t)) = do
      bs <- distBinds mbs
      t <- bindMany _bs
         $ (_t `equals` match_t) mt
      return (Alt' bs t)
      where
      distBinds [] = return []
      distBinds (Bind' l (mb, _b) : mbs) = do
        b <- mb
        bs <- bind (Bind l _b) (distBinds mbs)
        return (Bind' l b : bs)
      
      _bs = map (embedBind . map snd) mbs
      
      match_args = reverse 
        . map (Var . toEnum)
        $ [0..length mbs - 1]
      match_con = Inj n _ty
      match_t = unflattenApp (match_con : match_args)
      
  distM other = 
    sequence (map fst other)
    
instance Liftable Term where
  liftAt at = id
    . trackIndices at 
    . Fold.transformM liftVar
    where
    liftVar :: Term -> TrackIndices Index Term
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other
      
instance Liftable Bind where
  liftAt at (Bind l t) = Bind l (liftAt at t)
      
instance Substitutable Term where
  type Inner Term = Term

  substAt at with term = id
    . trackIndices (at, with)
    . Fold.transformM substVar
    $ term
    where
    substVar :: Term -> TrackIndices (Index, Term) Term
    substVar (Var var) = do
      (at, with) <- ask
      return $ case at `compare` var of
        -- Substitution occurs
        EQ -> with
        -- Substitution does not occur
        LT -> Var (pred var)
        GT -> Var var
    substVar other =
      return other
      
  free = id
    . trackIndices 0 
    . Fold.foldM freeR
    where
    freeR :: Term -> TrackIndices Index (Set Index)
    freeR (Var x) = do
      free_var_limit <- ask
      if x >= free_var_limit
      then return (Set.singleton (x - free_var_limit))
      else return mempty
    freeR _ = 
      return mempty
  
instance Substitutable Bind where
  type Inner Bind = Term
  substAt at with = modify boundType (substAt at with)
  free = Indices.free . get boundType
  
newtype TrackIndicesT r m a 
  = TrackIndicesT { runTrackIndicesT :: ReaderT r m a }
  deriving ( Monad )
  
type TrackIndices r = TrackIndicesT r Identity

trackIndicesT :: r -> TrackIndicesT r m a -> m a
trackIndicesT r = flip runReaderT r . runTrackIndicesT

trackIndices :: r -> TrackIndices r a -> a
trackIndices r = runIdentity . trackIndicesT r

instance Fail.Monad m => Fail.Monad (TrackIndicesT r m) where
  here = TrackIndicesT Fail.here
  
instance Monad m => MonadReader r (TrackIndicesT r m) where
  ask = TrackIndicesT ask
  local f = TrackIndicesT . local f . runTrackIndicesT
  
instance (Monad m, Liftable r) => Writable (TrackIndicesT r m) where
  bindAt at _ = local (liftAt at)
  equals _ _ = id
  
instance (Monoid w, Writable m) => Writable (WriterT w m) where
  bindAt at b = WriterT . bindAt at b . runWriterT
  equals t1 t2 = WriterT . equals t1 t2 . runWriterT
  
instance Monad m => Writable (ReaderT [Bind] m) where
  bindAt at b = 
      local 
    $ insertAt (convertEnum at) (liftAt at b) 
    . map (liftAt at)
  equals _ _ = id
  
instance Monad m => Readable (ReaderT [Bind] m) where 
  bindings = ask
  
instance Writable m => Writable (MaybeT m) where
  bindAt at b = MaybeT . bindAt at b . runMaybeT
  equals x y = MaybeT . equals x y . runMaybeT
  
instance Readable m => Readable (MaybeT m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Writable m => Writable (EitherT e m) where
  bindAt at b = EitherT . bindAt at b . runEitherT
  equals x y = EitherT . equals x y . runEitherT
  
instance Readable m => Readable (EitherT e m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Unifiable Term where
  find = (trackIndicesT 0 .) . uni
    where
    uniBind :: forall m . Fail.Monad m =>
      Bind -> Bind -> TrackIndicesT Index m (Unifier Term)
    uniBind = uni `on` get boundType
    
    uni :: forall m . Fail.Monad m => 
      Term -> Term -> TrackIndicesT Index m (Unifier Term)
    uni Absurd Absurd = return mempty
    uni Type Type = return mempty
    uni Set Set = return mempty
    uni (Var x1) (Var x2)
      | x1 == x2 = return mempty
    uni (Var idx) t2 = do
      free_var_limit <- ask
      -- If the variable on the left is not locally scoped
      -- then we can substitute it for something.
      -- We subtract 'free_var_limit' to get the index
      -- outside the bindings of this term.
      if idx < free_var_limit
      then Fail.here
      else do
        let lowered_idx = idx - free_var_limit
            lowered_t2 = Indices.lowerMany (fromEnum free_var_limit) t2
        return (Unifier.singleton lowered_idx lowered_t2)
    uni (Lam b1 t1) (Lam b2 t2) = do
      ub <- b1 `uniBind` b2
      ut <- local Indices.lift (t1 `uni` t2)
      Unifier.union ub ut
    uni (Pi b1 t1) (Pi b2 t2) = do
      ub <- b1 `uniBind` b2
      ut <- local Indices.lift (t1 `uni` t2)
      Unifier.union ub ut
    uni (Fix b1 t1) (Fix b2 t2) = do
      ub <- b1 `uniBind` b2
      ut <- local Indices.lift (t1 `uni` t2)
      Unifier.union ub ut
    uni (App l1 r1) (App l2 r2) = do
      uni1 <- uni l1 l2
      uni2 <- uni r1 r2
      Unifier.union uni1 uni2
    uni (Inj n1 t1) (Inj n2 t2) = do
      Fail.when (n1 /= n2)
      uni t1 t2
    uni (Ind b1 cs1) (Ind b2 cs2) = do
      ub <- uniBind b1 b2
      ucs <- local Indices.lift (zipWithM uniBind cs1 cs2)
      Unifier.unions (ub:ucs)
    uni (Case t1 ty1 alts1) (Case t2 ty2 alts2) = do
      Fail.when (length alts1 /= length alts2)
      ut <- uni t1 t2
      uty <- uni ty1 ty2
      ualts <- zipWithM uniAlt alts1 alts2
      Unifier.unions (ut:uty:ualts)
      where
      uniAlt :: Alt -> Alt -> TrackIndicesT Index m (Unifier Term)
      uniAlt (Alt bs1 t1) (Alt bs2 t2) = do
        ubs <- zipWithM uniBindL [0..] (bs1 `zip` bs2)
        ut <- local (Indices.liftMany (length bs1)) 
          $ uni t1 t2
        Unifier.unions (ut:ubs)
        where
        uniBindL n (b1, b2) = do
          local (Indices.liftMany n) (b1 `uniBind` b2)
    uni _ _ = Fail.here 
  
