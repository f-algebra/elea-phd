-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  module Elea.Term,
  replace, unfoldFix,
  decreasingArgs,
  applyCase,
  generaliseArgs,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Show
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Evaluation as Eval
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

unfoldFix :: Term -> Term
unfoldFix fix@(Fix _ fix_t) = 
  Indices.subst fix fix_t
  

-- | Replace all instances of one term with another within a term.
replace :: Term -> Term -> Term -> Term
replace me with = id
  . Env.trackIndices (me, with)
  . Fold.transformM doReplace
  where
  -- 'Env.TrackIndices' is needed to make sure indices
  -- are properly updated as we move inside 
  -- the term, e.g. if we pass inside a lambda.
  doReplace :: Term -> Env.TrackIndices (Term, Term) Term
  doReplace term = do
    (me, with) <- Env.tracked
    if term == me
    then return with
    else return term
  
  
-- | Returns the indices of the strictly decreasing arguments for
-- a given function. Undefined if not given a 'Fix'.
decreasingArgs :: Term -> [Int]
decreasingArgs (Fix fix_b fix_t) = 
  filter isDecreasing [0..length arg_bs - 1]
  where
  (arg_bs, fix_body) = flattenLam fix_t
  
  isDecreasing :: Int -> Bool
  isDecreasing arg_i = id
    . Env.trackIndices fix_f
    
    -- We track all terms which are 
    -- structurally smaller than our starting argument
    . Env.trackSmallerThan (Var arg_idx)
    $ Fold.allM decreasing fix_body
    where
    -- The deBrujin index of the lambda bound variable we are tracking
    arg_idx = enum (length arg_bs - (arg_i + 1))
    
    -- The deBrujin index of the fix bound function variable
    fix_f = enum (length arg_bs)
    
    decreasing :: 
      Term -> Env.TrackSmallerTermsT (Env.TrackIndices Index) Bool
    decreasing t@(App (Var f) args) = do
      fix_f <- Trans.lift Env.tracked
      if fix_f /= f
      then return True
      else Env.isSmaller (args !! arg_i)
    decreasing _ = 
      return True
      
      
-- | Take a case-of term and replace the result term down each branch
-- with the second term argument.
applyCase :: Term -> Term -> Term
applyCase (Case ind cse_t alts) inner_t = 
  Case ind cse_t (zipWith mkAlt [0..] alts)
  where
  mkAlt :: Int -> Alt -> Alt
  mkAlt n (Alt bs _) = Alt bs alt_t
    where
    -- Takes a term from outside the pattern match and lifts the 
    -- indices to what they should be within this branch
    liftHere = Indices.liftMany (nlength bs)
    
    -- The new alt-term is the given inner_t, with all occurrences of
    -- the pattern matched term replaced with the pattern it is matched
    -- to down this branch.
    pat = altPattern ind (enum n)
    alt_t = replace (liftHere cse_t) pat (liftHere inner_t)

 
-- | Generalise all the arguments of a term to fresh variables.
-- The first argument of the inner computation to run will lift 
-- indices by the number of new variables.
generaliseArgs :: (Indexed a, Substitutable t, Inner t ~ Term, Env.Readable m) =>
  Term -> ((a -> a) -> Term -> m t) -> m t
generaliseArgs (App func args) run = do
  -- Use the type of every arguments to generate bindings for our new 
  -- generalised variables.
  arg_tys <- mapM Type.get args
  let gen_bs = zipWith (\n -> Bind ("X" ++ show n)) [0..] arg_tys
        
  -- Run the inner computation
  done_t <- id
    . Env.bindMany (reverse gen_bs)
    $ run liftHere (App (liftHere func) new_vars)
    
  -- Reverse the generalisation
  return
    . foldr Indices.subst done_t
    $ reverse args
  where
  new_vars = map Var [0..elength args - 1]
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (elength args)
  
{-


-- | This is just the arguments of 'altPattern' at
-- the positions from 'recursiveInjArgs' 
recursivePatternArgs :: Type -> Nat -> Set Index
recursivePatternArgs ty n = 
  Set.map (fromVar . (args !!)) (recursiveInjArgs ty n)
  where
  args = tail (flattenApp (altPattern ty n))
  
-- | Returns the term in an 'Alt', lowered to be outside the bindings
-- of the 'Alt'. Returns 'Nothing' if the term contains
-- variables bound by the 'Alt'.
loweredAltTerm :: Alt -> Maybe Term
loweredAltTerm (Alt bs t) = 
  Indices.tryLowerMany (length bs) t
  
recursionDepth :: Term -> Int
recursionDepth (Fix _ _ rhs) = id
  . fromEnum
  . getMaximum
  . concatMap (flip matchDepth body . toEnum)
  $ [0..length args - 1]
  where
  (args, body) = flattenLam rhs 
  
  matchDepth :: Index -> Term -> Maximum Nat
  matchDepth idx = id
    . Env.trackIndices idx
    . Fold.isoFoldM restricted caseDepth
    where 
    caseDepth :: Term -> Env.TrackIndices Index (Maximum Nat)
    caseDepth (Case (Var x) ind_ty alts) = do
      t <- Env.tracked
      if x == t 
      then return (succ (concat (zipWith altDepth [0..] alts)))
      else return mempty
      where 
      altDepth alt_n (Alt _ alt_t) = id
        . concatMap (flip matchDepth alt_t) 
        $ recursivePatternArgs ind_ty alt_n
    caseDepth _ = 
      return mempty

-- | Given a set of branches from a pattern match, this checks whether
-- the recursive match variables are actually used. 
variablesFinitelyUsed :: Type -> [Alt] -> Bool
variablesFinitelyUsed ind_ty = and . zipWith recArgsUsed [0..]
  where
  recArgsUsed :: Nat -> Alt -> Bool
  recArgsUsed n (Alt _ alt_t) = 
    not (any isUsed args)
    where
    isUsed = (`Set.member` Env.isoFree restricted alt_t)
    args = recursivePatternArgs ind_ty n

variablesUnused :: Term -> Bool
variablesUnused (revertMatches -> Case _ _ alts) = 
  all unused alts
  where
  unused (Alt bs alt_t) = id
    . all (>= toEnum (length bs)) 
    $ Env.isoFree restricted alt_t
    

nthArgument :: Int -> Type -> Type
nthArgument n = id
  . Indices.lowerMany n
  . get boundType
  . (!! n)
  . fst
  . flattenPi 
  
doReplace :: Term -> Env.TrackIndices (Term, Term) Term
doReplace term = do
  (me, with) <- Env.tracked
  if term == me
  then return with
  else return term
  
-- | Replace all instances of one term with another.
replace :: ContainsTerms t => Term -> Term -> t -> t
replace me with = id
  . Env.trackIndices (me, with)
  . mapTermsM (Fold.transformM doReplace)
  
-- | Replace all non-restricted instances of one term with another.
replaceRestricted :: ContainsTerms t => Term -> Term -> t -> t
replaceRestricted me with = id
  . Env.trackIndices (me, with)
  . mapTermsM (Fold.isoTransformM restricted doReplace)
    
-- | The predicate will be applied with the subterm as the first argument.
containsAny :: (Term -> Term -> Bool) -> Term -> Term -> Bool
containsAny pred outer inner = id
  . Env.trackIndices inner
  . Fold.anyM (\t -> Env.trackeds (pred t))
  $ outer
    
-- | If the second argument is a subterm of the first
contains :: Term -> Term -> Bool
contains = containsAny (==)
  
-- | If the second argument can be unified with a subterm of the first
containsUnifiable :: Term -> Term -> Bool
containsUnifiable = 
  containsAny (\t t' -> not (isVar t) && Unifier.exists t t')

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
  gen_ty <- Err.noneM (Type.get gen_t)
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
revertMatchesWhenM :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m Term
revertMatchesWhenM when = Fold.isoTransformM restricted revert
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
    
revertMatchesWhen :: (Term -> Bool) -> Term -> Term
revertMatchesWhen when = runIdentity . revertMatchesWhenM (return . when)
    
revertMatches :: Term -> Term
revertMatches = revertMatchesWhen (const True)

-- | Some simple code shared by descendWhileM and BranchesOnly.
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

    
newtype BranchesOnly = BranchesOnly { notBranchesOnly :: Term }
  deriving ( Eq, Ord, Show )

-- A term isomorphism whose Tranformable instance only runs on the innermost
-- terms of pattern matches and lambda abstractions.
branchesOnly :: Fold.Iso Term BranchesOnly
branchesOnly = Fold.iso BranchesOnly notBranchesOnly

type instance Fold.Base BranchesOnly = Term'
  
instance Fold.Foldable BranchesOnly where
  project = fmap BranchesOnly . Fold.project . notBranchesOnly
  
instance Fold.Unfoldable BranchesOnly where
  embed = BranchesOnly . Fold.embed . fmap notBranchesOnly
  
instance Fold.FoldableM BranchesOnly where
  type FoldM BranchesOnly m = Env.Writable m
  distM = Fold.distM . fmap (second notBranchesOnly)

instance Fold.Transformable BranchesOnly where
  transformM f = id
    . liftM BranchesOnly 
    . Fold.selectiveTransformM (return . branches) f'
    . notBranchesOnly
    where
    f' = liftM notBranchesOnly . f . BranchesOnly
    
    branches :: Term -> (Bool, Term' (Bool, Term))
    branches (Case cse_t ind_ty alts) =
      (False, Case' (False, cse_t) (False, ind_ty) (map descendAlt alts))
    branches (Lam (Bind lbl ty) t) = 
      (False, Lam' (Bind' lbl (False, ty)) (True, t))
    branches term = 
      (True, fmap (\t -> (False, t)) (Fold.project term))

collectM :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    c <- Trans.lift . Trans.lift $ p t
    guard c
    offset <- Trans.lift Env.tracked
    return (Indices.lowerMany (fromEnum offset) t)
    
-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> Env.trackeds (== t))
  
buildCaseOfM :: forall m . Monad m => 
  Term -> Type -> (Nat -> m Term) -> m Term
buildCaseOfM cse_of ind_ty@(Typing.unfoldInd -> cons) mkAlt = do
  alts' <- zipWithM buildAlt [0..] cons
  return (Case cse_of ind_ty alts')
  where
  buildAlt :: Nat -> Bind -> m Alt
  buildAlt n bind = do
    alt_t <- mkAlt n
    return   
      . Alt bs 
      -- If we don't restrict here, it can wipe out stored previous 
      -- fix-fact steps in FixInfo.
      . replaceRestricted (liftHere cse_of) (altPattern ind_ty n)
      . liftHere 
      $ alt_t
    where
    liftHere = Indices.liftMany (length bs) 
    bs = fst . flattenPi . get boundType $ bind
    
buildCaseOf :: Term -> Type -> (Nat -> Term) -> Term
buildCaseOf cse_of ind_ty mkAlt = 
  runIdentity (buildCaseOfM cse_of ind_ty (Identity . mkAlt))
  
  
strictVars :: Term -> Set Index
strictVars (App (Fix _ _ fix_t) args) = id
  . Set.intersection (Indices.free args)
  . Env.trackIndices 0
  . Fold.isoFoldM restricted matchedUpon
  . Simp.run
  $ App fix_t args
  where
  matchedUpon :: Term -> Env.TrackIndices Index (Set Index)
  matchedUpon (Case (Var x) _ _) = do
    offset <- Env.tracked
    if x >= offset
    then return (Set.singleton (x - offset))
    else return mempty
  matchedUpon _ = return mempty
  

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
    fix_f <- Env.tracked
    guard (not (fix_f `Set.member` Indices.free term))
    return term
    
-- | Splits a context into two contexts, 
-- equivalent to the original if composed. 
-- The first is the application of any arguments to the gap,
-- the second is the rest. Will fail if the context is multi-hole with 
-- different arguments at each, if any arguments are contain non-free
-- variables, or if no arguments have been applied.
isolateArguments :: forall m . (Fail.Monad m, Env.Readable m) => 
  Context -> m (Context, Context)
isolateArguments ctx = do
  (term', args_set) <- id
    . Fail.fromMaybe
    . Env.trackIndices 0
    . runMaybeT
    . runWriterT
    . Fold.transformM isolate
    $ ctx_omega
  Fail.unless (Set.size args_set == 1)
  let args = head (toList args_set)
      inner_ctx = 
        Context.make inner_gap_ty (\t -> App t args)
  outer_gap_ty <- outerGapType args
  let outer_ctx =
        Context.make outer_gap_ty (\t -> substAt Indices.omega t term')
  return (inner_ctx, outer_ctx)
  where
  ctx_omega = Context.apply ctx (Var Indices.omega)
  inner_gap_ty = Context.gapType ctx
  
  -- The type of the gap in the outer context is just the type of the 
  -- original gap once all the arguments have been applied. 
  outerGapType :: [Term] -> m Type
  outerGapType args = id
    . liftM Indices.lower
    . Env.bindAt 0 (Bind Nothing inner_gap_ty)
    . Err.noneM
    . Type.get
    . App (Var 0)
    $ map Indices.lift args
  
  -- Find any application of the function (which we've replaced with omega)
  -- to a set of arguments, 'tell' these arguments to the writer,
  -- and remove them.
  isolate :: 
    Term -> WriterT (Set [Term]) (MaybeT (Env.TrackIndices Index)) Term
  isolate (App (Var f) args)
    | f == Indices.omega = do
      offset <- Env.tracked
      args' <- id 
        . Trans.lift
        . MaybeT 
        . return
        . mapM (Indices.tryLowerMany (fromEnum offset)) 
        $ args
      tell (Set.singleton args')
      return (Var Indices.omega)
  isolate other =
    return other
  
-- | Sets all the internal normal form flags to true.
normalised :: Term -> Term
normalised = id
  . runIdentity 
  . Fold.isoTransformM restricted (return . normal)
  where
  normal (Fix (FixInfo ms _ al) b t) = Fix (FixInfo ms True al) b t
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

expressMatches :: forall m . Env.Readable m => 
  ((Term, Term) -> Term -> Term -> m Bool) -> Term -> m Term
expressMatches when = 
  Fold.isoTransformM restricted express
  where
  express :: Term -> m Term
  express term = do
    ms <- liftM (map (second fst) . Map.toList) Env.matches
    term_ty <- Err.noneM (Type.get term)
    liftM (fromMaybe term)
      $ firstM (map (expressMatch term_ty) ms) 
    where
    expressMatch :: Type -> (Term, Term) -> m (Maybe Term) 
    expressMatch term_ty 
        match@(match_t, flattenApp -> Inj inj_n ind_ty : inj_args) = do
      now <- when match term expressed
      if now
      then return (Just expressed)
      else return Nothing
      where
      expressed = 
        Case match_t ind_ty (zipWith mkAlt [0..] ind_cons)
        where
        ind_cons = Typing.unfoldInd ind_ty
        
        mkAlt :: Nat -> Bind -> Alt
        mkAlt alt_n bind = Alt bs alt_t
          where
          bs = (fst . flattenPi . get boundType) bind
          liftHere = Indices.liftMany (length bs)
          
          arg_idxs = map (fromVar . liftHere) inj_args
          new_vars = map (Var . toEnum) (reverse [0..length bs - 1])
          
          alt_t 
            | alt_n /= inj_n = liftHere (Absurd term_ty)
            | otherwise = id
                . concatEndos (zipWith replaceAt arg_idxs new_vars)
                $ liftHere term

-- | Restricts where transformations will be applied within a 'Term'.
-- Only use for semantic preserving transformations.
newtype RestrictedTerm = Restrict { derestrict :: Term }
  deriving ( Eq, Ord, Show )
  
restricted :: Fold.Iso Term RestrictedTerm
restricted = Fold.iso Restrict derestrict
  
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
      | isPi term || isInd term = ignoreAll term
    select (Lam b t) = (True, Lam' (ignoreBind b) (True, t))
    select fix@(Fix inf b t)
      -- To simplify a fix it must not be in normal form,
      -- and must allow simplifications (@see 'FixInfo').
      | FixInfo _ False True <- inf = 
          (True, Fix' (ignoreInfo inf) (ignoreBind b) (True, t))
      | otherwise = ignoreAll fix
    select (Case cse_t ind_ty alts) = 
      (True, Case' (True, cse_t) (False, ind_ty) (map selectAlt alts))
      where
      selectAlt :: Alt -> Alt' (Bool, Term)
      selectAlt (Alt bs t) = 
        Alt' (map ignoreBind bs) (True, t)
    select term =
      -- selectAll is monadic, so we use runIdentity to strip the monad off.
      runIdentity (Fold.selectAll term)
-}
