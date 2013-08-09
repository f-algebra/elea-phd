-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  isBaseCase, isRecursiveInd,
  isFinite, isFinitelyUsed,
  recursiveInjArgs, recursivePatternArgs,
  replace, contains, nthArgument,
  collectM, occurrences,
  generalise, generaliseMany, 
  buildCaseOfM, buildCaseOf,
  revertMatchesWhen, descendWhileM,
  mapBranchesM, foldBranchesM,
  isProductive, normalised,
  minimumInjDepth, maximumInjDepth, injDepth,
  fragmentedUnifierExists,
  restrictedRewriteStepsM, restrictedRewriteM, 
  restrictedRewriteOnceM, restrictedTransformM,
  restrictedRewrite,
  module Elea.Term,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Index
import Elea.Term
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Typing as Typing
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Data.Set as Set
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

-- | For a given inductive type, return whether the constructor at that 
-- index is a base case.
isBaseCase :: Type -> Nat -> Bool
isBaseCase (Ind _ cons) inj_n = fromEnum inj_n
  |> (cons !!)
  |> get boundType 
  -- This flatten -> replace -> unflatten combo is to remove the return
  -- type of the constructor, since this will always be an instance of
  -- the inductive type.
  |> flattenPi
  |> second (const Set)
  |> uncurry unflattenPi
  |> Indices.free
  |> Set.member 0
  |> not
  
-- | For a given inductive type and constructor number this returns the
-- argument positions which are recursive.
recursiveInjArgs :: Type -> Nat -> Set Int
recursiveInjArgs (Ind _ cons) inj_n = id
  . mconcat
  . zipWith isRec [0..]
  . map (get boundType)
  $ args
  where
  con = cons !! fromEnum inj_n
  (args, _) = flattenPi (get boundType con)
  
  isRec :: Int -> Type -> Set Int
  isRec x ty 
    | toEnum x `Set.member` Indices.free ty = Set.singleton x
    | otherwise = mempty
    
-- | This is just the arguments of 'altPattern' at
-- the positions from 'recursiveInjArgs' 
recursivePatternArgs :: Type -> Nat -> Set Index
recursivePatternArgs ty n = 
  Set.map (fromVar . (args !!)) (recursiveInjArgs ty n)
  where
  args = tail (flattenApp (altPattern ty n))
  
isRecursiveInd :: Type -> Bool
isRecursiveInd ty@(Ind _ cons) = 
  not . all (isBaseCase ty) . map toEnum $ [0..length cons - 1]

-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (flattenApp -> Inj n ind_ty : args) = 
  all isFinite rec_args
  where
  rec_args = Set.map (args !!) (recursiveInjArgs ind_ty n)
isFinite _ = False

-- | Given a set of branches from a pattern match, this checks whether
-- the recursive match variables are actually used. 
isFinitelyUsed :: Type -> [Alt] -> Bool
isFinitelyUsed ind_ty = and . zipWith recArgsUsed [0..]
  where
  recArgsUsed :: Nat -> Alt -> Bool
  recArgsUsed n (Alt _ alt_t) = 
    not (any isUsed args)
    where
    isUsed = (`Set.member` Indices.free alt_t)
    args = recursivePatternArgs ind_ty n

nthArgument :: Int -> Type -> Type
nthArgument n = id
  . Indices.lowerMany n
  . get boundType
  . (!! n)
  . fst
  . flattenPi 

-- | Replace all instances of one term with another.
replace :: ContainsTerms t => Term -> Term -> t -> t
replace me with = id
  . Env.trackIndices (me, with)
  . mapTermsM (Fold.transformM apply)
  where
  apply :: Term -> Env.TrackIndices (Term, Term) Term
  apply term = do
    (me, with) <- ask
    if term == me
    then return with
    else return term

-- | If the first argument is a subterm of the second
contains :: Term -> Term -> Bool
contains sub = Env.trackIndices sub . Fold.anyM (\t -> asks (== t))
    
-- | Takes a term to generalise, and modifies a term to term function,
-- such that the supplied term is generalised going in,
-- and ungeneralised coming out. Just treat t1 and t2 as Term, 
-- I had to generalise it to t1 and t2 for annoying reasons.
generalise :: (Env.Readable m, ContainsTerms t1, ContainsTerms t2,
    KleisliShow t1, ShowM t1 m) => 
  Term -> (Index -> t1 -> m t2) -> (t1 -> m t2)
generalise gen_t transform term = do
  -- First we create a binding for the new variable by finding its type
  -- and creating a descriptive label.
  gen_ty <- Err.noneM (Typing.typeOf gen_t)
  lbl <- liftM (\t -> "{" ++ t ++ "}") (showM gen_t)
  let gen_b = Bind (Just lbl) gen_ty
  
  -- Generalise by replacing all instances of the term with a new variable
  -- at index 0 and make room for this index by lifting the original term.
  let term' = id 
        . replace (Indices.lift gen_t) (Var 0)
        $ Indices.lift term
  
  -- Apply our term to term function, with the new variable bound.
  term'' <- Env.bindAt 0 gen_b (transform 0 term') 
  
  -- Finally, we reverse the generalisation process.
  return 
    . Indices.lower
    . Indices.replaceAt 0 (Indices.lift gen_t)
    $ term''
    
generaliseMany :: forall m t1 t2 . 
  (Env.Readable m, ContainsTerms t1, ContainsTerms t2,
    KleisliShow t1, ShowM t1 m) => 
  [Term] -> ([Index] -> t1 -> m t2) -> (t1 -> m t2)
generaliseMany ts f = foldr gen f lifted_ts []
  where
  lifted_ts = zipWith Indices.liftMany [0..] ts
  
  gen :: Term -> ([Index] -> t1 -> m t2) -> ([Index] -> t1 -> m t2)
  gen t f ixs = 
    generalise t (\ix -> f (ix : map Indices.lift ixs))
    
-- | Pattern matches are automatically applied as replacements down
-- every branch. This will revert any such replacements of any term
-- that fulfils the given predicate.
revertMatchesWhen :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m  Term
revertMatchesWhen when = Fold.transformM revert
  where 
  revert :: Term -> m Term
  revert term@(Case cse_t ind_ty alts) = do
    here <- when cse_t
    if not here
    then return term
    else do
      let alts' = zipWith revertAlt [0..] alts
      return (Case cse_t ind_ty alts')
    where
    revertAlt :: Nat -> Alt -> Alt
    revertAlt n alt
      | isBaseCase ind_ty n = alt
    revertAlt n alt@(Alt bs alt_t) = 
      Alt bs alt_t'
      where
      rep_t = Indices.liftMany (length bs) cse_t
      alt_t' = replace (altPattern ind_ty n) rep_t alt_t
  revert other = 
    return other
  
-- | Some simple code shared by descendWhileM and mapBranchesM.
descendAlt :: Alt -> Alt' (Bool, Term)
descendAlt (Alt bs alt_t) = Alt' bs' (True, alt_t)
  where
  bs' = map (fmap (\t -> (False, t)) . projectBind) bs
  
-- | Decends into non-type subterms of a term while a predicate holds.
descendWhileM :: forall m . Fold.FoldM Term m => 
  (Term -> m Bool) -> (Term -> m Term) -> Term -> m Term
descendWhileM when = Fold.selectiveTransformM while
  where
  while :: Term -> m (Bool, Term' (Bool, Term))
  while term = do
    recurse <- when term
    if not recurse
    then return (True, fmap (\t -> (False, t)) (Fold.project term))
    else return (descend term)
    where
    descend :: Term -> (Bool, Term' (Bool, Term))
    descend (Case cse_t ind_ty alts) = 
      (False, Case' (True, cse_t) (False, ind_ty) (map descendAlt alts))
    descend (Lam (Bind lbl ty) t) = 
      (False, Lam' (Bind' lbl (False, ty)) (True, t))
      
    -- TODO: Missing cases here

-- | Applies a given transformation function to the innermost branches
-- of a term made of pattern matches, and the terms they match upon.
-- Will also move inside lambda abstractions.
mapBranchesM :: forall m . Env.Writable m => 
  (Term -> m Term) -> Term -> m Term
mapBranchesM = Fold.selectiveTransformM (return . branches)
  where
  branches :: Term -> (Bool, Term' (Bool, Term))
  branches (Case cse_t ind_ty alts) =
    (False, Case' (False, cse_t) (False, ind_ty) (map descendAlt alts))
  branches (Lam (Bind lbl ty) t) = 
    (False, Lam' (Bind' lbl (False, ty)) (True, t))
  branches term = 
    (True, fmap (\t -> (False, t)) (Fold.project term))

foldBranchesM :: (Env.Writable m, Monoid w) => (Term -> m w) -> Term -> m w
foldBranchesM f = execWriterT . mapBranchesM fold
  where
  fold t = do
    t |> f |> Trans.lift >>= tell
    return t

allBranchesM :: Env.Writable m => (Term -> m Bool) -> Term -> m Bool
allBranchesM p = liftM Monoid.getAll . foldBranchesM (liftM Monoid.All . p)

allBranches :: (Term -> Bool) -> Term -> Bool
allBranches p = runIdentity . allBranchesM (Identity . p)

collectM :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    c <- Trans.lift . Trans.lift $ p t
    guard c
    offset <- Trans.lift ask
    return (Indices.lowerMany (fromEnum offset) t)
    
-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> asks (== t))
  
buildCaseOfM :: forall m . Monad m => 
  Term -> Type -> (Nat -> m Term) -> m Term
buildCaseOfM cse_of ind_ty@(Typing.unfoldInd -> cons) mkAlt = do
  alts' <- zipWithM buildAlt [0..] cons
  return (Case cse_of ind_ty alts')
  where
  buildAlt :: Nat -> Bind -> m Alt
  buildAlt n bind = do
    alt_t <- mkAlt n $> Indices.liftMany (length bs)
    return (Alt bs alt_t)
    where
    bs = fst . flattenPi . get boundType $ bind
    
buildCaseOf :: Term -> Type -> (Nat -> Term) -> Term
buildCaseOf cse_of ind_ty mkAlt = 
  runIdentity (buildCaseOfM cse_of ind_ty (Identity . mkAlt))

-- | A function is "productive" if it will return something in HNF
-- if unrolled once. We implement this by checking for a constructor
-- topmost down every pattern match branch.
isProductive :: Term -> Bool
isProductive (Fix _ _ fix_t) = fix_t
  |> descendWhileM into productive
  |> runMaybeT
  |> Env.trackIndices 0
  |> isJust
  where
  into term = 
    return (isCase term || isLam term)
    
  productive :: Term -> MaybeT (Env.TrackIndices Index) Term
  productive term@(leftmost -> Inj {}) = return term
  productive term = do
    fix_f <- ask
    guard (not (fix_f `Set.member` Indices.free term))
    return term
    
fragmentedUnifierExists :: Term -> Term -> Bool
fragmentedUnifierExists t1 t2 =
  Env.trackIndices 0 (exists t1 t2)
  where
  orM, andM :: Monad m => [m Bool] -> m Bool
  orM = liftM or . sequence
  andM = liftM and . sequence
  
  existsB  = exists `on` get boundType
  
  exists :: Term -> Term -> Env.TrackIndices Index Bool
  -- This first bit is where it differs from 'Unifier.exists'.
  exists t1@(flattenApp -> app1) (flattenApp -> app2) 
    | length app2 >= 2 = 
      orM [ andM (zipWith exists app1 app2)
          , anyM (exists t1) app2 ]
  exists Type Type = return True
  exists Set Set = return True
  exists (Absurd t1) (Absurd t2) = exists t1 t2
  exists (Fix _ b1 t1) (Fix _ b2 t2) =
    andM [existsB b1 b2, local Indices.lift (exists t1 t2)]
  exists (Lam b1 t1) (Lam b2 t2) =
    andM [existsB b1 b2, local Indices.lift (exists t1 t2)]
  exists (Pi b1 t1) (Pi b2 t2) =
    andM [existsB b1 b2, local Indices.lift (exists t1 t2)]
  exists (Var x) (Var y) 
    | x == y = return True
  exists (Var x) t2 = do
    free_var_limit <- ask
    return (x >= free_var_limit)
  exists (Inj n1 t1) (Inj n2 t2) = do
    andM [return (n1 == n2), exists t1 t2]
  exists (Ind b1 cs1) (Ind b2 cs2) = 
    andM (zipWith existsB (b1:cs1) (b2:cs2))
  exists (Case t1 ty1 alts1) (Case t2 ty2 alts2) = 
    andM ( exists t1 t2 
         : exists ty1 ty2
         : zipWith existsAlt alts1 alts2 )
    where
    existsAlt (Alt bs1 t1) (Alt _ t2) =
      local (Indices.liftMany (length bs1)) (exists t1 t2)
  exists _ _ = return False

  
-- | Sets all the internal normal form flags to true.
normalised :: Term -> Term
normalised = runIdentity . restrictedTransformM (return . normal)
  where
  normal (Fix (FixInfo ms _) b t) = Fix (FixInfo ms True) b t
  normal other = other

injDepth :: (Enum w, Semigroup w) => Term -> w
injDepth (flattenApp -> Inj inj_n ind_ty : args) 
  | not (isBaseCase ind_ty inj_n) = 
    succ (sconcatMap injDepth rec_args)
  where
  rec_args = Set.map (args !!) (recursiveInjArgs ind_ty inj_n)
injDepth _ = toEnum 0
  
-- | Returns how unrolled a given HNF term is down the shortest recursive
-- path. This is a terrible explanation...
minimumInjDepth :: Term -> Int
minimumInjDepth = getMinimum . injDepth

maximumInjDepth :: Term -> Int
maximumInjDepth = getMaximum . injDepth


-- | Restricts where transformations will be applied within a 'Term'.
-- Only use for semantic preserving transformations.
newtype RestrictedTerm = Restrict { derestrict :: Term }
  deriving ( Eq, Ord, Show )
  
restrictRewrite :: Monad m => 
  (Term -> m (Maybe Term)) -> RestrictedTerm -> m (Maybe RestrictedTerm) 
restrictRewrite f = liftM (fmap Restrict) . f . derestrict

derestrictRewrite :: Monad m =>
  (RestrictedTerm -> m (Maybe RestrictedTerm)) -> Term -> m (Maybe Term)
derestrictRewrite f = liftM (fmap derestrict) . f . Restrict

restrictTransform :: Monad m => 
  (Term -> m Term) -> RestrictedTerm -> m RestrictedTerm
restrictTransform f = liftM Restrict . f . derestrict

derestrictTransform :: Monad m =>
  (RestrictedTerm -> m RestrictedTerm) -> Term -> m Term
derestrictTransform f = liftM derestrict . f . Restrict
  
restrictedTransformM :: Env.Writable m =>
  (Term -> m Term) -> Term -> m Term
restrictedTransformM = 
  derestrictTransform . Fold.transformM . restrictTransform

restrictedRewriteM :: Env.Writable m => 
  (Term -> m (Maybe Term)) -> Term -> m Term
restrictedRewriteM = 
  derestrictTransform . Fold.rewriteM . restrictRewrite
  
restrictedRewriteStepsM :: Env.Writable m =>
  [Term -> m (Maybe Term)] -> Term -> m Term
restrictedRewriteStepsM = 
  derestrictTransform . Fold.rewriteStepsM . map restrictRewrite 
  
restrictedRewriteOnceM :: Env.Writable m =>
  (Term -> m (Maybe Term)) -> Term -> m (Maybe Term)
restrictedRewriteOnceM =
  derestrictRewrite . Fold.rewriteOnceM . restrictRewrite
  
restrictedRewrite :: (Term -> Maybe Term) -> Term -> Term
restrictedRewrite f = runIdentity . restrictedRewriteM (return . f)
  
type instance Fold.Base RestrictedTerm = Term'
  
instance Fold.Foldable RestrictedTerm where
  project = fmap Restrict . Fold.project . derestrict
  
instance Fold.Unfoldable RestrictedTerm where
  embed = Restrict . Fold.embed . fmap derestrict
  
instance Fold.FoldableM RestrictedTerm where
  type FoldM RestrictedTerm m = Env.Writable m
  distM = Fold.distM . fmap (second derestrict) 
  
instance Fold.Transformable RestrictedTerm where
  transformM f = id
    . liftM Restrict 
    . Fold.selectiveTransformM (return . select) f'
    . derestrict
    where
    f' = liftM derestrict . f . Restrict
    
    ignoreBind :: Bind -> Bind' (Bool, Term)
    ignoreBind = fmap (\t -> (False, t)) . projectBind
    
    ignoreInfo :: FixInfo -> FixInfo' (Bool, Term)
    ignoreInfo = fmap (\t -> (False, t)) . projectFixInfo
    
    ignoreAll :: Term -> (Bool, Term' (Bool, Term))
    ignoreAll term = 
      (False, fmap (\t -> (False, t)) (Fold.project term))
    
    select :: Term -> (Bool, Term' (Bool, Term))
    -- I haven't implemented any type transformations yet.
    select term 
      | isPi term || isInj term || isInd term = ignoreAll term
    select (Lam b t) = (True, Lam' (ignoreBind b) (True, t))
    select fix@(Fix inf b t) 
      | get normalForm inf = ignoreAll fix
      | otherwise = 
          (True, Fix' (ignoreInfo inf) (ignoreBind b) (True, t))
    select (Case cse_t ind_ty alts) = 
      (True, Case' (True, cse_t) (False, ind_ty) (map selectAlt alts))
      where
      selectAlt :: Alt -> Alt' (Bool, Term)
      selectAlt (Alt bs t) = 
        Alt' (map ignoreBind bs) (True, t)
    select term =
      -- selectAll is monadic, so we use runIdentity to strip the monad off.
      runIdentity (Fold.selectAll term)


