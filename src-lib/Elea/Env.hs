-- | Type environments, but also a lot of class instances for 'Term',
-- since they require type environments.
{-# LANGUAGE UndecidableInstances #-}
module Elea.Env 
(
  Write (..), Read (..), 
  boundAt, bindingDepth,
  
  MatchRead (..), 
  findMatches,
  
  TrackMatches, trackMatches,
  
  Tracks (..), trackeds,
  AlsoTrack, alsoTrack, alsoWith,
  TrackIndices, TrackIndicesT,
  trackIndices, trackIndicesT,
  liftTracked, 
  
  TrackOffset, TrackOffsetT,
  trackOffset, trackOffsetT,
  offset, liftByOffset, lowerByOffset, lowerableByOffset,
  
  bind, bindMany,
  isoFree, isoShift,
  
  empty, emptyT,
  
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
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Index as Indices
import qualified Elea.Unifier as Unifier 
import qualified Elea.Foldable as Fold
import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set
import qualified Data.Map as Map

class Monad m => Write m where
  -- | Bind a variable index to a type within the environment
  bindAt :: Index -> Bind -> m a -> m a
  
  -- | Declare that the first term has been pattern matched to the second
  matched :: Term -> Term -> m a -> m a
  
bind :: Write m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Write m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

-- | Whether you can read variable bindings from the environment. 
-- Since variables are indices, it's really a lookup function 'Nat -> Bind',
-- but this is just a list.
class Write m => Read m where
  bindings :: m [Bind]
  
-- | Lookup the type of a variable. 
boundAt :: Read m => Index -> m Bind
boundAt at = do
  bs <- bindings
  if at >= length bs
  then error $ "Cannot retrieve the binding for index " ++ show at
    ++ " in an environment with only " ++ show (length bs :: Int) 
    ++ " bindings."
  else return (bs !! enum at)

-- | Returns the number of indices that have been bound.
bindingDepth :: Read m => m Int
bindingDepth = liftM length bindings
  
-- | Whether you can read locally bound pattern matches from
-- an environment monad
class Read m => MatchRead m where
  matches :: m [(Term, Term)]
  
findMatches :: MatchRead m => (Term -> Bool) -> m [(Term, Term)]
findMatches p = liftM (filter (p . fst)) matches
  

instance Write m => Fold.FoldableM m Term where
  -- To fold over a 'Term' we require that our monad implement a 'Write'
  -- environment. This environment can then be correctly updated as 
  -- we move into the syntax tree of the term.
  distM (Lam' b (mt, _)) =
    return (Lam' b) `ap` bind b mt
  distM (Fix' i b (mt, _)) =
    return (Fix' i b) `ap` bind b mt
  distM (Case' ind (mt, cse_t) malts) = do
    t <- mt
    alts <- zipWithM distAltM malts [0..]
    return (Case' ind t alts)
    where
    distAltM (Alt' bs (mt, _)) alt_n = do
      t <- id
        . bindMany bs
        . matched (Indices.liftMany (length bs) cse_t) pat
        $ mt
      return (Alt' bs t)
      where
      pat = altPattern ind alt_n
  distM other = 
    sequence (fmap fst other)
    
instance Write m => Fold.TransformableM m Term where
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
    
  -- Need to make sure the variables within 'FixInfo's get shifted manually.
  -- As the terms stored within them are not exposed by the Term' type, they
  -- will not be reached by 'isoTransformM.
  shiftVar (Fix info b t) = do
    offset <- tracked
    let info' = modify fusedMatches (shiftMatches offset) info
    return (Fix info' b t)
    where
    shiftMatches :: Index -> Set (Set Term) -> Set (Set Term)
    shiftMatches offset = id
      . Set.map
      . Set.map
      $ trackIndices offset 
      . Fold.isoTransformM iso shiftVar
  shiftVar other = 
    return other
  
instance Indexed Term where
  free = isoFree id
  shift = isoShift id
  
instance Indexed Alt where
  free (Alt bs alt_t) = id
    -- Drop the remaining variables to their value outside of the alt bindings
    . Set.map (Indices.lowerMany (length bs))
    -- Take the free variables of the term, minus those bound by the alt
    $ Indices.free alt_t `Set.difference` not_free
    where
    not_free :: Set Index
    not_free = Set.fromList (map enum [0..length bs - 1])
    
  shift f (Alt bs alt_t) = 
    Alt bs (Indices.shift f' alt_t)
    where
    -- The first index not bound by the alt
    min_idx :: Index
    min_idx = length bs
    
    -- The shifting function, altered to be within the alt bindings
    f' idx 
      | idx < min_idx = idx
      | otherwise = f (idx - min_idx) + min_idx
      
instance Indexed FixInfo where
  free = concatMap (concatMap Indices.free) . get fusedMatches
  shift f = modify fusedMatches (Set.map (Set.map (Indices.shift f)))

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
    -- Fixpoint information is not reached by the regular transformations
    -- but substitution needs to still affect it, as it changes indices.
    substVar (Fix (FixInfo ms) b t) = do
      sub <- tracked
      let ms' = substMatches sub ms
      return (Fix (FixInfo ms') b t) 
      where 
      substMatches :: (Index, Term) -> Set (Set Term) -> Set (Set Term)
      substMatches sub = id
        . Set.map
        . Set.map
        $ trackIndices sub 
        . Fold.transformM substVar
    substVar other = 
      return other
      
-- | If you just need a simple type environment, use the reader
-- monad over a stack of type bindings. This function will strip this
-- monad off, by starting with an empty stack.
empty :: Reader [Bind] a -> a
empty = runIdentity . emptyT

emptyT :: Monad m => ReaderT [Bind] m a -> m a
emptyT = flip runReaderT mempty

-- | Anything that tracks indices as we move within something that binds
-- indices.
class (Monad m, Indexed r) => Tracks r m | m -> r where
  tracked :: m r
  liftTrackedMany :: Nat -> m a -> m a
  
trackeds :: Tracks r m => (r -> a) -> m a 
trackeds f = liftM f tracked

liftTracked :: Tracks r m => m a -> m a 
liftTracked = liftTrackedMany 1

instance Tracks r m => Tracks r (MaybeT m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapMaybeT (liftTrackedMany n)
  
instance (Monoid w, Tracks r m) => Tracks r (WriterT w m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapWriterT (liftTrackedMany n)
  
-- Place 'AlsoTrack' over the top of a 'Write' environment monad.
-- 'AlsoTrack' will capture all changes to the environment and pass them
-- along to the inner monad. 
-- If you don't want your inner monad to not receive the changes 
-- (or it is just not 'Write'), then use 'TrackIndicesT'.
newtype AlsoTrack r m a
  = AlsoTrack { runAlsoTrack :: ReaderT r m a }
  deriving ( Monad, MonadTrans )
  
instance (Indexed r, Monad m) => Tracks r (AlsoTrack r m) where
  tracked = AlsoTrack ask
  liftTrackedMany n =
    AlsoTrack . local (Indices.liftMany n) . runAlsoTrack

mapAlsoTrack :: Monad m => (m a -> n b) -> 
  (AlsoTrack r m a -> AlsoTrack r n b)
mapAlsoTrack f = AlsoTrack . mapReaderT f . runAlsoTrack

alsoTrack :: r -> AlsoTrack r m a -> m a
alsoTrack r = flip runReaderT r . runAlsoTrack

alsoWith :: Monad m => (r' -> r) -> AlsoTrack r m a -> AlsoTrack r' m a
alsoWith f = AlsoTrack . withReaderT f . runAlsoTrack


-- Instances for 'AlsoTrack'
instance (Write m, Indexed r) => Write (AlsoTrack r m) where
  bindAt at b = id
    . AlsoTrack 
    . local (liftAt at)
    . runAlsoTrack 
    . mapAlsoTrack (bindAt at b)
    
  matched t w = 
    mapAlsoTrack (matched t w)
    
instance (Read m, Indexed r) => Read (AlsoTrack r m) where
  bindings = Trans.lift bindings
    
instance Fail.Can m => Fail.Can (AlsoTrack r m) where
  here = Trans.lift Fail.here
  catch = mapAlsoTrack Fail.catch
  
instance Defs.Read m => Defs.Read (AlsoTrack r m) where
  lookupTerm = Trans.lift . Defs.lookupTerm
  lookupType = Trans.lift . Defs.lookupType
  lookupName = Trans.lift . Defs.lookupName
  
instance Fail.Can m => Fail.Can (IdentityT m) where
  here = Trans.lift Fail.here
  catch = mapIdentityT Fail.catch


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

offset :: (Enum e, Tracks e m) => m Nat
offset = trackeds enum

liftByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m a
liftByOffset x = liftM (flip Indices.liftMany x) offset

lowerByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m a
lowerByOffset x = liftM (flip Indices.lowerMany x) offset

lowerableByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m Bool
lowerableByOffset x = liftM (flip Indices.lowerableBy x) offset 

newtype TrackMatches m a
  = TrackMatches { runTrackMatches :: ReaderT [(Term, Term)] m a }
  deriving ( Monad, MonadTrans )
  
mapTrackMatches :: Monad m => (m a -> n b) -> 
  (TrackMatches m a -> TrackMatches n b)
mapTrackMatches f = TrackMatches . mapReaderT f . runTrackMatches

trackMatches :: TrackMatches m a -> m a
trackMatches = flip runReaderT mempty . runTrackMatches

localMatches :: Monad m =>
  ([(Term, Term)] -> [(Term, Term)]) -> TrackMatches m a -> TrackMatches m a
localMatches f = TrackMatches . local f . runTrackMatches

instance Write m => Write (TrackMatches m) where
  bindAt at b = id
    . localMatches (map (liftAt at *** liftAt at))
    . mapTrackMatches (bindAt at b)
  
  matched t con_t = id
    . localMatches (++ [(t, con_t)])
    . mapTrackMatches (matched t con_t)
    
instance Read m => Read (TrackMatches m) where
  bindings = Trans.lift bindings
  
instance Read m => MatchRead (TrackMatches m) where
  matches = TrackMatches ask
  
instance Fail.Can m => Fail.Can (TrackMatches m) where
  here = Trans.lift Fail.here
  catch = mapTrackMatches Fail.catch
  
instance Defs.Read m => Defs.Read (TrackMatches m) where
  lookupTerm = Trans.lift . Defs.lookupTerm
  lookupType = Trans.lift . Defs.lookupType
  lookupName = Trans.lift . Defs.lookupName

-- Various instances for 'Write' and 'Read'
  
instance Write Identity where
  bindAt _ _ = id
  matched _ _ = id

instance Monad m => Write (IdentityT m) where
  -- The 'IdentityT' monad just ignores all the written type information
  bindAt _ _ = id
  matched _ _ = id

instance (Monoid w, Write m) => Write (WriterT w m) where
  bindAt at b = mapWriterT (bindAt at b)
  matched t w = mapWriterT (matched t w)
  
instance (Monoid w, Read m) => Read (WriterT w m) where
  bindings = Trans.lift bindings
             
instance Monad m => Write (ReaderT [Bind] m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ _ = id
  
instance Monad m => Read (ReaderT [Bind] m) where 
  bindings = ask
  
instance Write m => Write (MaybeT m) where
  bindAt at b = mapMaybeT (bindAt at b)
  matched t w = mapMaybeT (matched t w)
  
instance Read m => Read (MaybeT m) where
  bindings = Trans.lift bindings
  
instance MatchRead m => MatchRead (MaybeT m) where
  matches = Trans.lift matches
  
instance Write m => Write (EitherT e m) where
  bindAt at b = mapEitherT (bindAt at b)
  matched t w = mapEitherT (matched t w)
  
instance Read m => Read (EitherT e m) where
  bindings = Trans.lift bindings

instance Write m => Write (StateT s m) where
  bindAt at b = mapStateT (bindAt at b)
  matched t w = mapStateT (matched t w)
  
instance Read m => Read (StateT s m) where
  bindings = Trans.lift bindings
  
  
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
    
instance (Show Term, Write m) => Write (TrackSmallerTermsT m) where
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
           | term `Set.member` set = Set.insert with (set ++ rec_args)
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
    uni :: forall m . Fail.Can m => 
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
    uni (Fix _ b1 t1) (Fix _ b2 t2) = 
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
      Fail.assert (length alts1 == (length alts2 :: Int))
      ut <- uni t1 t2
      ualts <- zipWithM uniAlt alts1 alts2
      Unifier.unions (ut:ualts) 
      where
      uniAlt :: Alt -> Alt -> TrackIndicesT Index m (Unifier Term)
      uniAlt (Alt bs1 t1) (Alt bs2 t2) =
        liftTrackedMany (length bs1) (uni t1 t2)
    uni _ _ = Fail.here 
    
  apply uni = id
    . trackOffset
    . Fold.transformM replace
    where
    replace :: Term -> TrackOffset Term
    replace (Var x) = do
      n <- offset
      if x < enum n
      then return (Var x)
      else do
        case Map.lookup (x - enum n) uni of 
          Nothing -> return (Var x)
          Just t -> return (Indices.liftMany n t)
    replace other = 
      return other

