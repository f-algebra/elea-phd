-- | Type environments, but also a lot of class instances for 'Term',
-- since they require type environments.
{-# LANGUAGE UndecidableInstances #-}
module Elea.Env 
(
  Writable (..), Readable (..),
  AlsoTrack, alsoTrack, alsoWith,
  TrackIndices, TrackIndicesT,
  liftTracked, tracked, trackeds, 
  trackIndices, trackIndicesT,
  
  TrackOffset, TrackOffsetT,
  trackOffset, trackOffsetT,
  offset, liftHere,
  
  bind, bindMany,
  isoFree, isoShift,
  
  empty,
  
  TrackSmallerTermsT, TrackSmallerTerms, 
  trackSmallerThan, isSmaller,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Unifier ( Unifiable, Unifier )
import qualified Elea.Type as Type
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Index as Indices
import qualified Elea.Unifier as Unifier 
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Control.Monad.Trans as Trans

class Monad m => Writable m where
  -- | Bind a variable index to a type within the environment
  bindAt :: Index -> Bind -> m a -> m a
  
  -- | Declare that the first term has been pattern matched to the second
  matched :: Term -> Term -> m a -> m a
  
bind :: Writable m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Writable m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

class Writable m => Readable m where
  bindings :: m [Bind]
  bindings = error "No bindings stored."
  
  boundAt :: Index -> m Bind
  boundAt at = liftM (!! fromEnum at) bindings
  
  -- | Returns the number of indices that have been bound.
  bindingDepth :: m Int
  bindingDepth = liftM length bindings
  
instance Writable m => Fold.FoldableM m Term where
  -- To fold over a 'Term' we require that our monad implement a 'Writable'
  -- environment. This environment can then be correctly updated as 
  -- we move into the syntax tree of the term.
  distM (Lam' b (mt, _)) =
    return (Lam' b) `ap` bind b mt
  distM (Fix' b (mt, _)) =
    return (Fix' b) `ap` bind b mt
  distM (Case' ind (mt, cse_t) malts) = do
    t <- mt
    alts <- zipWithM distAltM malts [0..]
    return (Case' ind t alts)
    where
    distAltM (Alt' bs (mt, _)) alt_n = do
      t <- id
        . bindMany bs
        . matched (Indices.liftMany (nlength bs) cse_t) pat
        $ mt
      return (Alt' bs t)
      where
      pat = altPattern ind alt_n
  distM other = 
    sequence (fmap fst other)
    
instance Writable m => Fold.TransformableM m Term where
  -- Use the default instance for transformM
    
isoFree :: 
    (Fold.TransformableM (WriterT (Set Index) (TrackIndices Index)) t) =>
  Fold.Iso Term t -> Term -> Set Index
isoFree iso = id
  . trackIndices 0
  . Fold.isoFoldM iso freeR
  where
  freeR :: Term -> TrackIndices Index (Set Index)
  freeR (Var x) = do
    at <- tracked
    if x >= at
    then return (Set.singleton (x - at))
    else return mempty
  freeR _ = 
    return mempty
    
isoShift :: (Fold.TransformableM (TrackIndices Index) t) => 
  Fold.Iso Term t -> (Index -> Index) -> Term -> Term
isoShift iso f = id
  . trackIndices 0
  . Fold.isoTransformM iso shiftVar
  where
  shiftVar :: Term -> TrackIndices Index Term
  shiftVar (Var x) = do
    at <- tracked
    let x' | x >= at = f (x - at) + at
           | otherwise = x
    return (Var x')
  shiftVar other = 
    return other
  
instance Indexed Term where
  free = isoFree id
  shift = isoShift id
       
instance Substitutable Term where
  type Inner Term = Term

  substAt at with = id
    . trackIndices (at, with)
    . Fold.transformM substVar
    where
    substVar :: Term -> TrackIndices (Index, Term) Term
    substVar (Var var) = do
      (at, with) <- tracked
      return $ case at `compare` var of
        -- Substitution occurs
        EQ -> with
        -- Substitution does not occur
        LT -> Var (pred var)
        GT -> Var var
    substVar other =
      return other
      
-- | If you just need a simple type environment, use the reader
-- monad over a stack of type bindings. This function will strip this
-- monad off, by starting with an empty stack.
empty :: Reader [Bind] a -> a
empty = flip runReader mempty
   
-- Place 'AlsoTrack' over the top of a 'Writable' environment monad.
-- 'AlsoTrack' will capture all changes to the environment and pass them
-- along to the inner monad. 
-- If you don't want your inner monad to not receive the changes 
-- (or it is just not 'Writable'), then use 'TrackIndicesT'.
newtype AlsoTrack r m a
  = AlsoTrack { runAlsoTrack :: ReaderT r m a }
  deriving ( Monad, MonadTrans )
  
tracked :: Monad m => AlsoTrack r m r
tracked = AlsoTrack ask

trackeds :: Monad m => (a -> b) -> AlsoTrack a m b
trackeds f = liftM f tracked

liftTrackedMany :: (Monad m, Indexed r) => 
  Nat -> AlsoTrack r m a -> AlsoTrack r m a
liftTrackedMany n = AlsoTrack . local (Indices.liftMany n) . runAlsoTrack
  
liftTracked :: (Monad m, Indexed r) => AlsoTrack r m a -> AlsoTrack r m a
liftTracked = liftTrackedMany 1

mapAlsoTrack :: Monad m => (m a -> m a) -> 
  (AlsoTrack r m a -> AlsoTrack r m a)
mapAlsoTrack f = AlsoTrack . mapReaderT f . runAlsoTrack

alsoTrack :: r -> AlsoTrack r m a -> m a
alsoTrack r = flip runReaderT r . runAlsoTrack

alsoWith :: Monad m => (r' -> r) -> AlsoTrack r m a -> AlsoTrack r' m a
alsoWith f = AlsoTrack . withReaderT f . runAlsoTrack


-- Instances for 'AlsoTrack'
instance (Writable m, Indexed r) => Writable (AlsoTrack r m) where
  bindAt at b = id
    . AlsoTrack 
    . local (liftAt at)
    . runAlsoTrack 
    . mapAlsoTrack (bindAt at b)
    
  matched t w = 
    mapAlsoTrack (matched t w)
    
instance (Readable m, Indexed r) => Readable (AlsoTrack r m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
    
instance Fail.Monad m => Fail.Monad (AlsoTrack r m) where
  here = Trans.lift Fail.here
  
instance Fail.Monad m => Fail.Monad (IdentityT m) where
  here = Trans.lift Fail.here


-- | To stop effects reaching the inner monad we
-- just wrap it in an 'IdentityT'.
type TrackIndicesT r m = AlsoTrack r (IdentityT m)
type TrackIndices r = TrackIndicesT r Identity

trackIndicesT :: r -> TrackIndicesT r m a -> m a
trackIndicesT r = runIdentityT . flip runReaderT r . runAlsoTrack

trackIndices :: r -> TrackIndices r a -> a
trackIndices r = runIdentity . trackIndicesT r


-- | For just tracking how many variables have been bound.
type TrackOffsetT m = TrackIndicesT Index m
type TrackOffset = TrackOffsetT Identity

trackOffsetT :: Monad m => TrackOffsetT m a -> m a
trackOffsetT = trackIndicesT 0

trackOffset :: TrackOffset a -> a
trackOffset = runIdentity . trackOffsetT

offset :: Monad m => TrackOffsetT m Nat
offset = trackeds enum

liftHere :: (Monad m, Indexed a) => a -> TrackOffsetT m a
liftHere = liftM Indices.liftMany offset


-- Various instances for 'Writable' and 'Readable'
  
instance Writable Identity where
  bindAt _ _ = id
  matched _ _ = id

instance Monad m => Writable (IdentityT m) where
  -- The 'IdentityT' monad just ignores all the written type information
  bindAt _ _ = id
  matched _ _ = id

instance (Monoid w, Writable m) => Writable (WriterT w m) where
  bindAt at b = mapWriterT (bindAt at b)
  matched t w = mapWriterT (matched t w)
  
instance (Monoid w, Readable m) => Readable (WriterT w m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Monad m => Writable (ReaderT [Bind] m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ _ = id
  
instance Monad m => Readable (ReaderT [Bind] m) where 
  bindings = ask
  
instance Writable m => Writable (MaybeT m) where
  bindAt at b = mapMaybeT (bindAt at b)
  matched t w = mapMaybeT (matched t w)
  
instance Readable m => Readable (MaybeT m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Writable m => Writable (EitherT e m) where
  bindAt at b = mapEitherT (bindAt at b)
  matched t w = mapEitherT (matched t w)
  
instance Readable m => Readable (EitherT e m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth

instance Writable m => Writable (StateT s m) where
  bindAt at b = mapStateT (bindAt at b)
  matched t w = mapStateT (matched t w)
  
instance Readable m => Readable (StateT s m) where
  bindings = Trans.lift bindings
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
  
-- Tracking pattern matches

-- | Stores a list of terms structurally smaller than a given object.
data Smaller a
  = Smaller { smallerThan :: a
            , smaller :: Set a }
  deriving ( Eq, Show )
                  
instance (Ord a, Indexed a) => Indexed (Smaller a) where
  free (Smaller than set) = 
    free than ++ concatMap free set
    
  shift f (Smaller than set) = 
    Smaller (shift f than) (Set.mapMonotonic (shift f) set)
    
type TrackSmallerTermsT = ReaderT (Smaller Term)
type TrackSmallerTerms = TrackSmallerTermsT Identity

trackSmallerThan :: Term -> TrackSmallerTermsT m a -> m a
trackSmallerThan than = flip runReaderT (Smaller than mempty) 

isSmaller :: (Show Term, Monad m) => Term -> TrackSmallerTermsT m Bool
isSmaller term = do
  set <- asks smaller
  return (isFinite term || term `Set.member` set)
    
instance (Show Term, Writable m) => Writable (TrackSmallerTermsT m) where
  bindAt idx t = id
    . mapReaderT (bindAt idx t)
    . local (liftAt idx)
    
  matched term with@(flattenApp -> Con ind n : args) = id
    . mapReaderT (matched term with) 
    . local addMatch
    where
    rec_args = id
      . Set.fromList
      . map (args !!) 
      $ Type.recursiveArgs ind n
     
    addMatch (Smaller than set) = 
      Smaller than set'
      where
      set' | term == than = set ++ rec_args
           | term `Set.member` set = Set.insert term (set ++ rec_args)
           | otherwise = set

        
-- Other stuff
  
instance ContainsTerms Term where
  mapTermsM = ($)

instance Unifiable Term where
  find t1 t2 = do
    possible_uni <- trackIndicesT 0 (t1 `uni` t2)
    -- Need to test out the unifier. It could be invalid if at some
    -- points a variable needs to be replaced, but at others it stays the same.
    Fail.when (Unifier.apply possible_uni t1 /= t2)
    return possible_uni
    where
    uni :: forall m . Fail.Monad m => 
      Term -> Term -> TrackIndicesT Index m (Unifier Term)
    uni (Absurd ty1) (Absurd ty2) = do
      Fail.assert (ty1 == ty2)
      return mempty
    -- TODO: Experimental concept w.r.t. unification of absurdities below.
    -- Leads to interesting simplifications. Come back to this later.
    -- uni _ (Absurd _) = return mempty
    uni (Var x1) (Var x2)
      | x1 == x2 = return mempty
    uni (Var idx) t2 = do
      free_var_limit <- tracked
      -- If the variable on the left is not locally scoped
      -- then we can substitute it for something.
      -- We subtract 'free_var_limit' to get the index
      -- outside the bindings of this term.
      if idx < free_var_limit
      then Fail.here
      else do
        let lowered_idx = idx - free_var_limit
            lowered_t2 = Indices.lowerMany (enum free_var_limit) t2
        return (Unifier.singleton lowered_idx lowered_t2)
    uni (Lam b1 t1) (Lam b2 t2) =
      -- Since we are inside a binding the indices go up by one, 
      -- so we call 'liftTracked'.
      liftTracked (t1 `uni` t2)
    uni (Fix b1 t1) (Fix b2 t2) = 
      liftTracked (t1 `uni` t2)
    uni (App f1 xs1) (App f2 xs2) = do
      uni_f <- uni f1 f2
      uni_xs <- zipWithM uni xs1 xs2
      Unifier.unions (uni_f : uni_xs)
    uni (Con ty1 n1) (Con ty2 n2) = do
      Fail.when (n1 /= n2)
      Fail.when (ty1 /= ty2)
      return mempty
    uni (Case ind1 t1 alts1) (Case ind2 t2 alts2) = do
      Fail.when (ind1 /= ind2) 
      Fail.assert (length alts1 == length alts2)
      ut <- uni t1 t2
      ualts <- zipWithM uniAlt alts1 alts2
      Unifier.unions (ut:ualts) 
      where
      uniAlt :: Alt -> Alt -> TrackIndicesT Index m (Unifier Term)
      uniAlt (Alt bs1 t1) (Alt bs2 t2) =
        liftTrackedMany (nlength bs1) (uni t1 t2)
    uni _ _ = Fail.here 

