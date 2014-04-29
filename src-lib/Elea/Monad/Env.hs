{-# LANGUAGE UndecidableInstances #-}
-- | Class instances for the constraints in "Elea.Monad.Env".
-- Also a lot of general class instances for 'Term's, since they 
-- require environment tracking term traversal.
module Elea.Monad.Env 
(
  module Elea.Monad.Env.Class,
  
  TrackMatches, trackMatches,
  mapTrackMatches,
 
  AlsoTrack, alsoTrack, alsoWith,
  TrackIndices, TrackIndicesT,
  trackIndices, trackIndicesT,
  mapAlsoTrack,
  
  TrackOffset, TrackOffsetT,
  trackOffset, trackOffsetT,
  
  isoFree, isoShift,
  empty, emptyT,
  isBaseCase,
  
  TrackSmallerTermsT, TrackSmallerTerms, 
  trackSmallerThan, isSmaller,
)
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Type ( ContainsTypes (..) )
import Elea.Unification ( Unifiable, Unifier )
import Elea.Monad.Env.Class
import qualified Elea.Type as Type
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Index as Indices
import qualified Elea.Unification as Unifier 
import qualified Elea.Unification.Map as UMap
import qualified Elea.Foldable as Fold
import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set
import qualified Data.Map as Map


instance Write m => Fold.FoldableM m Term where
  -- To fold over a 'Term' we require that our monad implement a 'Write'
  -- environment. This environment can then be correctly updated as 
  -- we move into the syntax tree of the term.
  distM (Lam' b (mt, _)) =
    return (Lam' b) `ap` bind b mt
  distM (Case' (mt, cse_t) malts) = do
    t <- mt
    alts <- mapM distAltM malts
    return (Case' t alts)
    where
    distAltM (Alt' con bs (mt, _)) = do
      t <- id
        . bindMany bs
        . matchHere
        $ mt
      return (Alt' con bs t)
      where
      matchHere = 
        matched (Indices.liftMany (length bs) cse_t)
                (constructorPattern con)
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
  freeR (Var x _) = do
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
  shiftVar (Var x args) = do
    at <- tracked
    let x' | x >= at = f (x - at) + at
           | otherwise = x
    return (Var x' args)
  shiftVar other = 
    return other
  
instance Indexed Term where
  free = isoFree id
  shift = isoShift id
  
instance Indexed Alt where
  free (Alt _ bs alt_t) = id
    -- Drop the remaining variables to their value outside of the alt bindings
    . Set.map (Indices.lowerMany (length bs))
    -- Take the free variables of the term, minus those bound by the alt
    $ Indices.free alt_t `Set.difference` not_free
    where
    not_free :: Set Index
    not_free = Set.fromList (map enum [0..length bs - 1])
    
  shift f (Alt con bs alt_t) = 
    Alt con bs (Indices.shift f' alt_t)
    where
    -- The first index not bound by the alt
    min_idx :: Index
    min_idx = length bs
    
    -- The shifting function, altered to be within the alt bindings
    f' idx 
      | idx < min_idx = idx
      | otherwise = f (idx - min_idx) + min_idx

instance Substitutable Term where
  type Inner Term = Term

  substAt at with = id
    . trackIndices (at, with)
    . Fold.transformM substVar
    where
    substVar :: Term -> TrackIndices (Index, Term) Term
    substVar (Var x args) = do
      (at, with) <- tracked
      return $ case at `compare` x of
        -- Substitution occurs
        EQ -> apply with args
        -- Substitution does not occur
        LT -> Var (pred x) args
        GT -> Var x args
    substVar other = 
      return other

instance ContainsTypes Term where
  mapTypesM f = runIgnoreT . Fold.transformM mapTy
    where
    f' = IgnoreT . IdentityT . f
    
    -- We use IgnoreT to absorb the type bindings written by transformM.
    -- See 'Elea.Monad.Env.Write'.
    mapTy (Lam b t) = do
      b' <- mapTypesM f' b
      return (Lam b' t)
    mapTy (Unr ty) = do
      ty' <- f' ty
      return (Unr ty')
    mapTy (Con con args) = do
      con' <- mapTypesM f' con
      return (Con con' args)
    mapTy (Case cse_t alts) = do
      alts' <- mapM mapAlt alts
      return (Case cse_t alts')
      where
      mapAlt (Alt con bs alt_t) = do
        con' <- mapTypesM f' con
        bs' <- mapM (mapTypesM f') bs
        return (Alt con' bs' alt_t)
    mapTy term =
      return term
      
instance ContainsTypes Equation where
  mapTypesM f (Equals name bs t1 t2) = do
    bs' <- mapM (mapTypesM f) bs
    t1' <- mapTypesM f t1
    t2' <- mapTypesM f t2
    return (Equals name bs' t1' t2')
    
    
-- | The writable type environment which ignores everything written to it.
-- Importantly it also blocks this information from flowing to the inner monad.
newtype IgnoreT m a = IgnoreT { runIgnoreT' :: IdentityT m a }
  deriving ( Monad, MonadTrans )

type Ignore = IgnoreT Identity

runIgnoreT :: IgnoreT m a -> m a
runIgnoreT =  runIdentityT . runIgnoreT'

runIgnore :: Ignore a -> a
runIgnore = runIdentity . runIgnoreT

instance Monad m => Write (IgnoreT m) where
  bindAt _ _ = id
  matched _ _ = id
  
instance Fail.Can m => Fail.Can (IgnoreT m) where
  here = Trans.lift Fail.here
  catch = IgnoreT . Fail.catch . runIgnoreT'
  
      
-- | If you just need a simple type environment, use the reader
-- monad over a stack of type bindings. This function will strip this
-- monad off, by starting with an empty stack.
empty :: Reader [Bind] a -> a
empty = runIdentity . emptyT

emptyT :: Monad m => ReaderT [Bind] m a -> m a
emptyT = flip runReaderT mempty

  
-- | Whether a given term has been matched to a base case
-- down this branch.
isBaseCase :: Matches m => Term -> m Bool
isBaseCase term = do
  ms <- matches
  case lookup term ms of
    Nothing -> return False
    Just con_t -> 
      allM isBaseCase (recursiveConArgs con_t)

  
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
    
instance (Bindings m, Indexed r) => Bindings (AlsoTrack r m) where
  bindings = Trans.lift bindings
    
instance Fail.Can m => Fail.Can (AlsoTrack r m) where
  here = Trans.lift Fail.here
  catch = mapAlsoTrack Fail.catch
  
instance Discovery.Tells m => Discovery.Tells (AlsoTrack r m) where
  tell = Trans.lift . Discovery.tell

instance Discovery.Listens m => Discovery.Listens (AlsoTrack r m) where
  listen = mapAlsoTrack Discovery.listen


-- | To stop effects reaching the inner monad we
-- just wrap it in an 'IgnoreT'.
type TrackIndicesT r m = AlsoTrack r (IgnoreT m)
type TrackIndices r = AlsoTrack r Ignore

trackIndicesT :: r -> TrackIndicesT r m a -> m a
trackIndicesT r = runIgnoreT . flip runReaderT r . runAlsoTrack

trackIndices :: r -> TrackIndices r a -> a
trackIndices r = runIdentity . trackIndicesT r


-- | For just tracking how many variables have been bound.
type TrackOffsetT m = TrackIndicesT Index m
type TrackOffset = TrackOffsetT Identity

trackOffsetT :: Monad m => TrackOffsetT m a -> m a
trackOffsetT = trackIndicesT 0

trackOffset :: TrackOffset a -> a
trackOffset = runIdentity . trackOffsetT

-- | A simple monad for tracking pattern matches
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
    
instance Bindings m => Bindings (TrackMatches m) where
  bindings = Trans.lift bindings
  
instance Write m => Matches (TrackMatches m) where
  matches = TrackMatches ask
  
instance Fail.Can m => Fail.Can (TrackMatches m) where
  here = Trans.lift Fail.here
  catch = mapTrackMatches Fail.catch
  
instance Tracks r m => Tracks r (TrackMatches m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapTrackMatches (liftTrackedMany n)

instance Discovery.Tells m => Discovery.Tells (TrackMatches m) where
  tell = Trans.lift . Discovery.tell

instance Discovery.Listens m => Discovery.Listens (TrackMatches m) where
  listen = mapTrackMatches Discovery.listen
  

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
    
  matched term with = id
    . mapReaderT (matched term with) 
    . local addMatch
    where
    rec_args = Set.fromList (recursiveConArgs with)
     
    addMatch (Smaller than set) = 
      Smaller than set'
      where
      set' | term == than = set ++ rec_args
           | term `Set.member` set = Set.insert with (set ++ rec_args)
           | otherwise = set

        
-- Other stuff

instance Unifiable Term where
  find t1 t2 = do
    possible_uni <- trackOffsetT (t1 `uni` t2)
    -- Need to test out the unifier. It could be invalid if at some
    -- points a variable needs to be replaced, but at others it stays the same.
    Fail.when (Unifier.apply possible_uni t1 /= t2)
    return possible_uni
    where
    zipWithUni :: Fail.Can m 
      => [Term] -> [Term] -> TrackOffsetT m (Unifier Term)
    zipWithUni ts1 ts2 = do
      unis <- zipWithM uni ts1 ts2
      Unifier.unions unis
    
    uni :: forall m . Fail.Can m => 
      Term -> Term -> TrackOffsetT m (Unifier Term)
    uni (Var f1 xs1) (Var f2 xs2)
      | f1 == f2 = zipWithUni xs1 xs2 
    uni (Var f1 xs1) t2 = do
      -- If the variable on the left is not locally scoped
      -- then we can substitute it for something.
      f1' <- tryLowerByOffset f1
      (f2, xs2) <- stripArgs (length xs1) t2
      f2' <- tryLowerByOffset f2
      arg_uni <- zipWithUni xs1 xs2
      let f_uni = Unifier.singleton f1' f2'
      Unifier.union f_uni arg_uni
    uni (Lam b1 t1) (Lam b2 t2) =
      -- Since we are inside a binding the indices go up by one, 
      -- so we call 'liftTracked'.
      liftTracked (t1 `uni` t2)
    uni (Def n1 xs1) (Def n2 xs2) = do
      Fail.unless (n1 == n2)
      zipWithUni xs1 xs2
    uni (Con c1 xs1) (Con c2 xs2) = do
      Fail.unless (c1 == c2)
      zipWithUni xs1 xs2
    uni (Unr ty1) (Unr ty2) = do
      Fail.assert (ty1 == ty2)
      return mempty
    uni (Case t1 alts1) (Case t2 alts2) = do
      ut <- uni t1 t2
      ualts <- zipWithM uniAlt alts1 alts2
      Unifier.unions (ut:ualts) 
      where
      uniAlt :: Alt -> Alt -> TrackOffsetT m (Unifier Term)
      uniAlt (Alt con1 bs1 t1) (Alt con2 bs2 t2) = do
        Fail.assert (con1 == con2)
        liftTrackedMany (length bs1) (uni t1 t2)
    uni _ _ = Fail.here 
    
  apply uni = id
    . trackOffset
    . Fold.transformM replace
    where
    replace :: Term -> TrackOffset Term
    replace (Var x xs) = do
      n <- offset
      if x < enum n
      then return (Var x xs)
      else do
        case Map.lookup (x - enum n) uni of 
          Nothing -> return (Var x xs)
          Just t -> return (apply (Indices.liftMany n t) xs)
    replace other = 
      return other
      
  gcompare t1 t2 = trackOffset (comp t1 t2)
    where
    -- We make heavy use of the lexicographical 'Ordering' 'Monoid'.
    
    zipWithComp :: [Term] -> [Term] -> TrackOffset Ordering
    zipWithComp xs ys = liftM mconcat (zipWithM comp xs ys)
    
    comp :: Term -> Term -> TrackOffset Ordering
    comp (Var x xs) (Var y ys) = do
      c_args <- zipWithComp xs ys
      free_x <- lowerableByOffset x
      free_y <- lowerableByOffset y
      if free_x || free_y
      then return (EQ ++ c_args)
      else return (x `compare` y ++ c_args)
    comp (Var x xs) t = do
      free_x <- lowerableByOffset x
      if argumentCount t < length xs
        || not (free_x)
      then return LT
      else do
        let Just (f, ys) = stripArgs (length xs) t
        f_free <- lowerableByOffset f
        if not f_free
        then return LT 
        else do
          c_args <- zipWithComp xs ys
          return (EQ ++ c_args)
    comp t (Var y ys) = do
      free_y <- lowerableByOffset y
      if argumentCount t < length ys
        || not (free_y)
      then return GT
      else do
        let Just (f, xs) = stripArgs (length ys) t
        f_free <- lowerableByOffset f
        if not f_free
        then return GT 
        else do
          c_args <- zipWithComp xs ys
          return (EQ ++ c_args)
    comp (Unr _) (Unr _) = return EQ
    comp (Unr _) _ = return LT
    comp _ (Unr _) = return GT
    comp (Def a xs) (Def b ys) = do
      c_args <- zipWithComp xs ys
      return (a `compare` b ++ c_args)
    comp (Def {}) _ = return LT
    comp _ (Def {}) = return GT
    comp (Lam _ t) (Lam _ t') = liftTracked (comp t t')
    comp (Lam {}) _ = return LT
    comp _ (Lam {}) = return GT
    comp (Con c1 xs) (Con c2 ys) = do
      c_args <- zipWithComp xs ys
      return (c1 `compare` c2 ++ c_args)
    comp (Con {}) _ = return LT
    comp _ (Con {}) = return GT
    comp (Case t1 alts1) (Case t2 alts2) = do
      ct <- comp t1 t2
      calts <- zipWithM compAlts alts1 alts2
      return (mconcat (ct:calts))
      where 
      compAlts (Alt c1 bs1 t1) (Alt c2 bs2 t2) = do
        ct <- liftTrackedMany (length bs1) (comp t1 t2)
        return (c1 `compare` c2 ++ ct)
      
instance Indexed Constraint where
  free = free . get constrainedTerm
  shift f = modify constrainedTerm (shift f)
      
instance Substitutable Constraint where 
  type Inner Constraint = Term
  substAt x t = 
    modify constrainedTerm (substAt x t)
  
instance Unifiable Constraint where
  apply uni =
    modify constrainedTerm (Unifier.apply uni)
    
  find (Constraint con1 t1) (Constraint con2 t2) = do
    Fail.unless (con1 == con2)
    Unifier.find t1 t2
    
  gcompare (Constraint con1 t1) (Constraint con2 t2) =
    con1 `compare` con2 ++ t1 `Unifier.gcompare` t2

