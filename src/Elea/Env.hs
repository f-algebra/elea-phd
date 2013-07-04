-- | Type environments, but also a lot of class instances for 'Term',
-- since they require type environments.
module Elea.Env 
(
  Writable (..), Readable (..),
  TrackIndices, TrackIndicesT (..),
  trackIndices, trackIndicesT,
  bind, bindMany, matchedWith,
  AlsoTrack, alsoTrack, alsoWith,
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
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans

class Monad m => Writable m where
  bindAt :: Index -> Bind -> m a -> m a
  equals :: Term -> Term -> m a -> m a
  forgetMatches :: m a -> m a
  
bind :: Writable m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Writable m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

class Writable m => Readable m where
  bindings :: m [Bind]
  matches :: m Matches
  
  boundAt :: Index -> m Bind
  boundAt at = liftM (!! fromEnum at) bindings
  
  -- | Returns the number of indices that have been bound.
  bindingDepth :: m Int
  bindingDepth = liftM length bindings
  
matchedWith :: Readable m => Term -> m (Maybe Term)
matchedWith t = liftM (Map.lookup t) matches
  
instance Fold.FoldableM Term where
  -- To fold over a 'Term' we require that our monad implement a writable
  -- environment. This environment can then be correctly updated as 
  -- we move into the syntax tree of the term.
  type FoldM Term m = Writable m
  
  distM (Lam' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Lam' (Bind' l ty) t)
  distM (Fix' minf (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) (forgetMatches mt)
    inf <- forgetMatches (distInfo minf)
    return (Fix' inf (Bind' l ty) t)
    where
    distInfo (FixInfo' mms) = do
      ms <- mapM distMatch mms
      return (FixInfo' ms)
      where
      distMatch :: Monad m => ((m a, b), Nat) -> m (a, Nat)
      distMatch ((ma, _), n) = liftM (\a -> (a, n)) ma
  distM (Pi' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Pi' (Bind' l ty) t)
  distM (Ind' (Bind' l (mty, _ty)) mcs) = do
    ty <- mty
    cs <- bind (Bind l _ty) (mapM (sequence . map fst) mcs)
    return (Ind' (Bind' l ty) cs)
  distM (Case' (mt, _t) (mty, _ty) malts) = do
    t <- mt
    ty <- mty
    alts <- zipWithM (distAltM _t _ty) [0..] malts
    return (Case' t ty alts)
  distM other = 
    sequence (map fst other)
    
instance Fold.Transformable Term where
  -- Provided my own instance of this to make sure "case-of" terms are
  -- transformed before their branches.
  transformM f (Case cse_t ind_ty alts) = do
    cse_t' <- Fold.transformM f cse_t
    ind_ty' <- Fold.transformM f ind_ty
    alts' <- id
      . liftM (map embedAlt)
      . zipWithM (distAltM cse_t' ind_ty') [0..]
      . map (fmap (\t -> (Fold.transformM f t, t)))
      . map projectAlt
      $ alts
    f (Case cse_t' ind_ty' alts')
  transformM f other = id
    . join
    . liftM (f . Fold.embed)
    . Fold.distM 
    . fmap (\x -> (Fold.transformM f x, x))
    . Fold.project 
    $ other

-- | The distribution law for 'Alt's is relatively complex, since we have to
-- store any bound patterns as equalities in the environment (@see 'equals').
distAltM :: Writable m => 
  Term -> Type -> Nat -> Alt' (m a, Term) -> m (Alt' a)
distAltM cse_t ind_ty alt_n (Alt' mbs (mt, _)) = do
  bs <- distBinds mbs
  t <- id
    . bindMany _bs
    . equals cse_t' (altPattern ind_ty' alt_n)
    $ mt
  return (Alt' bs t)
  where
  distBinds [] = return []
  distBinds (Bind' l (mb, _b) : mbs) = do
    b <- mb
    bs <- bind (Bind l _b) (distBinds mbs)
    return (Bind' l b : bs)
  
  _bs = map (embedBind . map snd) mbs  
  cse_t' = liftMany (length _bs) cse_t
  ind_ty' = liftMany (length _bs) ind_ty

instance Indexed Term where
  free = id
    . trackIndices 0 
    . Fold.foldM freeR
    where
    freeR :: Term -> TrackIndices Index (Set Index)
    freeR (Var x) = do
      at <- ask
      if x >= at
      then return (Set.singleton (x - at))
      else return mempty
    freeR _ = 
      return mempty

  shift f = id
    . trackIndices 0
    . Fold.transformM shiftVar
    where
    shiftVar :: Term -> TrackIndices Index Term
    shiftVar (Var x) = do
      at <- ask
      let x' | x >= at = f (x - at) + at
             | otherwise = x
      return (Var x')
    shiftVar other = 
      return other
      
instance Indexed Bind where
  free (Bind _ t) = Indices.free t
  shift f (Bind l t) = Bind l (Indices.shift f t)
  
instance Indexed FixInfo where
  free (FixInfo ms) = concatMap (Indices.free . fst) ms
  shift f (FixInfo ms) = FixInfo (map (first (Indices.shift f)) ms)
      
instance Substitutable Term where
  type Inner Term = Term

  substAt at with = id
    . trackIndices (at, with)
    . Fold.transformM substVar
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

instance Substitutable Bind where
  type Inner Bind = Term
  substAt at with = modify boundType (substAt at with)
  
newtype TrackIndicesT r m a 
  = TrackIndicesT { runTrackIndicesT :: ReaderT r m a }
  deriving ( Monad, MonadReader r, MonadTrans )
  
type TrackIndices r = TrackIndicesT r Identity

trackIndicesT :: r -> TrackIndicesT r m a -> m a
trackIndicesT r = flip runReaderT r . runTrackIndicesT

trackIndices :: r -> TrackIndices r a -> a
trackIndices r = runIdentity . trackIndicesT r

instance Fail.Monad m => Fail.Monad (TrackIndicesT r m) where
  here = Trans.lift Fail.here
  
instance (Monad m, Indexed r) => Writable (TrackIndicesT r m) where
  bindAt at _ = local (liftAt at)
  equals _ _ = id
  forgetMatches = id

-- | An environment monad which tracks indices but also passes environment
-- bindings to an inner monad (see the 'Writable' class for details).
newtype AlsoTrack r m a
  = AlsoTrack { runAlsoTrack :: ReaderT r m a }
  deriving ( Monad, MonadReader r, MonadTrans )
  
mapAlsoTrack :: Monad m => (m a -> m a) -> 
  (AlsoTrack r m a -> AlsoTrack r m a)
mapAlsoTrack f = AlsoTrack . mapReaderT f . runAlsoTrack

alsoTrack :: r -> AlsoTrack r m a -> m a
alsoTrack r = flip runReaderT r . runAlsoTrack

alsoWith :: Monad m => (r' -> r) -> AlsoTrack r m a -> AlsoTrack r' m a
alsoWith f = AlsoTrack . withReaderT f . runAlsoTrack
   
instance (Writable m, Indexed r) => Writable (AlsoTrack r m) where
  bindAt at b = local (liftAt at) . mapAlsoTrack (bindAt at b)
  equals x y = mapAlsoTrack (equals x y)
  forgetMatches = mapAlsoTrack forgetMatches
  
instance (Readable m, Indexed r) => Readable (AlsoTrack r m) where
  bindings = Trans.lift bindings
  matches = Trans.lift matches
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Fail.Monad m => Fail.Monad (AlsoTrack r m) where
  here = Trans.lift Fail.here
  
instance (Monoid w, Writable m) => Writable (WriterT w m) where
  bindAt at b = WriterT . bindAt at b . runWriterT
  equals t1 t2 = WriterT . equals t1 t2 . runWriterT
  forgetMatches = WriterT . forgetMatches . runWriterT
  
instance Monad m => Writable (ReaderT [Bind] m) where
  bindAt at b = 
      local 
    $ insertAt (convertEnum at) (liftAt at b) 
    . map (liftAt at)
  equals _ _ = id
  forgetMatches = id
  
instance Monad m => Readable (ReaderT [Bind] m) where 
  bindings = ask
  matches = return mempty
  
instance Writable m => Writable (MaybeT m) where
  bindAt at b = MaybeT . bindAt at b . runMaybeT
  equals x y = MaybeT . equals x y . runMaybeT
  forgetMatches = MaybeT . forgetMatches . runMaybeT
  
instance Readable m => Readable (MaybeT m) where
  bindings = Trans.lift bindings
  matches = Trans.lift matches
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Writable m => Writable (EitherT e m) where
  bindAt at b = EitherT . bindAt at b . runEitherT
  equals x y = EitherT . equals x y . runEitherT
  forgetMatches = EitherT . forgetMatches . runEitherT
  
instance Readable m => Readable (EitherT e m) where
  bindings = Trans.lift bindings
  matches = Trans.lift matches
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Writable m => Writable (StateT s m) where
  bindAt at b = StateT . (bindAt at b .) . runStateT
  equals x y = StateT . (equals x y .) . runStateT
  forgetMatches = StateT . (forgetMatches .) . runStateT
  
instance Readable m => Readable (StateT s m) where
  bindings = Trans.lift bindings
  matches = Trans.lift matches
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Writable Identity where
  bindAt _ _ = id
  equals _ _ = id
  forgetMatches = id
  
instance Readable Identity where
  bindings = return mempty
  matches = return mempty
  
instance ContainsTerms Term where
  mapTermsM = ($)

instance ContainsTerms Bind where
  mapTermsM f = modifyM boundType f

instance Unifiable Term where
  find t1 t2 = do
    possible_uni <- trackIndicesT 0 (uni t1 t2)
    -- Need to test out the unifier. It could be invalid if at some
    -- points a variable needs to be replaced, but at others it stays the same.
    Fail.when (Unifier.apply possible_uni t1 /= t2)
    return possible_uni
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
    uni (Fix _ b1 t1) (Fix _ b2 t2) = do
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

