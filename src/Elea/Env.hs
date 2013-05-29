-- | Type environments, but also a lot of class instances for 'Term',
-- since they require type environments.
module Elea.Env 
(
  Writable (..), Readable (..),
  TrackIndices, TrackIndicesT (..),
  trackIndices, trackIndicesT,
  bind, bindMany,
  replaceTerm,
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
  
  matchedWith :: Term -> m (Maybe Term)
  matchedWith t = liftM (Map.lookup t) matches

  
-- | Replace all instances of one term with another within terms.
replaceTerm :: ContainsTerms t => Term -> Term -> t -> t
replaceTerm me with = id
  . trackIndices (me, with)
  . mapTermsM (Fold.transformM apply)
  where
  apply :: Term -> TrackIndices (Term, Term) Term
  apply term = do
    (me, with) <- ask
    if term == me
    then return with
    else return term
  
instance Fold.FoldableM Term where
  type FoldM Term m = Writable m
  
  distM (Lam' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Lam' (Bind' l ty) t)
  distM (Fix' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) (forgetMatches mt)
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
    distAlt n (Alt' mbs (mt, _)) = do
      bs <- distBinds mbs
      t <- id
        . bindMany _bs
        . equals (liftMany (length _bs) _t) (altPattern _ty n)
        $ mt
      return (Alt' bs t)
      where
      distBinds [] = return []
      distBinds (Bind' l (mb, _b) : mbs) = do
        b <- mb
        bs <- bind (Bind l _b) (distBinds mbs)
        return (Bind' l b : bs)
      
      _bs = map (embedBind . map snd) mbs
      
  distM other = 
    sequence (map fst other)
    
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
  free (Bind _ t) = free t
  shift f (Bind l t) = Bind l (Indices.shift f t)
      
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
  
instance (Monad m, Indexed r) => Writable (TrackIndicesT r m) where
  bindAt at _ = local (liftAt at)
  equals _ _ = id
  forgetMatches = id
  
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
  matchedWith = Trans.lift . matchedWith
  
instance Writable m => Writable (EitherT e m) where
  bindAt at b = EitherT . bindAt at b . runEitherT
  equals x y = EitherT . equals x y . runEitherT
  forgetMatches = EitherT . forgetMatches . runEitherT
  
instance Readable m => Readable (EitherT e m) where
  bindings = Trans.lift bindings
  matches = Trans.lift matches
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  matchedWith = Trans.lift . matchedWith
  
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
  
