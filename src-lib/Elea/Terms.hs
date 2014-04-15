-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  module Elea.Term,
  branches,
  replace,
  unfoldFix,
  unwrapFix,
  collectM, collect,
  decreasingArgs,
  decreasingAppArgs,
  applyCase,
  generaliseArgs,
  generaliseTerms,
  generaliseUninterpreted,
  tuple,
  equation,
  isEquation,
  isFiniteMatch,
  expressFreeVariable,
  expressFreeVariables,
  revertMatchesWhenM, 
  revertMatchesWhen, 
  revertMatches,
  occurrences,
  isSubterm,
  alreadyFused,
  findContext,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

-- | A wrapper around 'Term' for the 'branchesOnly' isomorphism.
newtype BranchesOnly = BranchesOnly { notBranchesOnly :: Term }
  deriving ( Eq, Ord, Show )

-- | A 'Term' isomorphism whose 'Fold.Transformable' instance 
-- only runs on the innermost terms of pattern matches and lambda abstractions.
-- We can use this isomorphism for the /iso/ style functions in 'Elea.Foldable'.
-- For example, to call @Fold.all p@ over just branches we use
-- > Fold.isoAll branchesOnly p t
-- > where p :: Term -> Bool, t :: Term
branches :: Fold.Iso Term BranchesOnly
branches = Fold.iso BranchesOnly notBranchesOnly

type instance Fold.Base BranchesOnly = Term'
  
instance Fold.Foldable BranchesOnly where
  project = fmap BranchesOnly . Fold.project . notBranchesOnly
  
instance Fold.Unfoldable BranchesOnly where
  embed = BranchesOnly . Fold.embed . fmap notBranchesOnly
  
instance Env.Write m => Fold.FoldableM m BranchesOnly where
  distM = Fold.distM . fmap (second notBranchesOnly)

instance Env.Write m => Fold.TransformableM m BranchesOnly where
  transformM f = id
    . liftM BranchesOnly 
    . Fold.selectiveTransformM (return . branches) f'
    . notBranchesOnly
    where
    f' = liftM notBranchesOnly . f . BranchesOnly
    
    branches :: Term -> (Bool, Term' Bool)
    branches (Case ind cse_t alts) =
      (False, Case' ind False (map descAlt alts))
      where 
      descAlt (Alt bs alt_t) = Alt' bs True
    branches (Lam b t) = 
      (False, Lam' b True)
    branches term = 
      (True, fmap (const False) (Fold.project term))
      
      
unfoldFix :: Term -> Term
unfoldFix fix@(Fix _ _ fix_t) = 
  Indices.subst fix fix_t
  
  
-- | Unfolds a fixpoint a given number of times and leaves the fix variable
-- uninterpreted.
unwrapFix :: Nat -> Term -> Term
unwrapFix 0 (Fix _ _ fix_t) = fix_t
unwrapFix n fix@(Fix _ _ fix_t) = 
  -- We use 'replaceAt' here so as not to lower the indices of the unwrapped
  -- definition, since we are leaving the fixpoint variable in.
  Indices.replaceAt 0 (unwrapFix (n - 1) fix) fix_t
  
  
-- | Collect terms which fulfil a given predicate. 
-- The variables of these terms must be free outside the original term,
-- and will be automatically lowered to the correct indices.
collectM :: forall m . Env.Write m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    condition <- (Trans.lift . Trans.lift . p) t
    Fail.unless condition
    
    lowerable <- Env.lowerableByOffset t
    Fail.unless lowerable
    
    Env.lowerByOffset t
    
-- | See 'collectM'
collect :: (Term -> Bool) -> Term -> Set Term
collect p = runIdentity . collectM (Identity . p)
    

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
  

-- | A wrapped around 'decreasingArgs' which takes a fixpoint with arguments
-- applied and removes any return indices which are greater than the length
-- of the arguments.
-- Helps stop 'decreasingArgs' causing errors on partially applied 
-- fixpoints.
decreasingAppArgs :: Term -> [Int]
decreasingAppArgs (App fix args) = 
  filter (length args >) (decreasingArgs fix)
  

-- | Returns the indices of the strictly decreasing arguments for
-- a given function. Undefined if not given a 'Fix'.
decreasingArgs :: Term -> [Int]
decreasingArgs (Fix _ fix_b fix_t) = 
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
    fix_f = length arg_bs
    
    decreasing :: 
      Term -> Env.TrackSmallerTermsT (Env.TrackIndices Index) Bool
    decreasing t@(App (Var f) args) = do
      fix_f <- Trans.lift Env.tracked
      if fix_f /= f || arg_i >= length args
      then return True
      else Env.isSmaller (args `nth` arg_i)
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
    liftHere = Indices.liftMany (length bs)
    
    -- The new alt-term is the given inner_t, with all occurrences of
    -- the pattern matched term replaced with the pattern it is matched
    -- to down this branch.
    pat = altPattern ind (enum n)
    alt_t = replace (liftHere cse_t) pat (liftHere inner_t)

 
-- | Generalise all the arguments of a term to fresh variables.
-- The first argument of the inner computation to run will lift 
-- indices by the number of new variables.
generaliseArgs :: forall m a .
    (Env.Read m, Defs.Read m, Substitutable a, Inner a ~ Term) =>
  Term -> (Indices.Shift -> Term -> m a) -> m a
generaliseArgs (App func args) run = do
  -- Use the type of every arguments to generate bindings for our new 
  -- generalised variables.
  gen_bs <- mapM makeBind [0..length args - 1] 
        
  -- Run the inner computation
  done_t <- id
    . Env.bindMany (reverse gen_bs)
    $ run liftHere (App (liftHere func) new_vars)
    
  -- Reverse the generalisation
  return
    . foldr Indices.subst done_t
    . zipWith Indices.liftMany [0..]
    $ reverse args
  where
  new_vars = map Var [0..length args - 1]
  
  makeBind :: Int -> m Bind
  makeBind n
    | Var x <- args `nth` n = Env.boundAt x
  makeBind n = do
    ty <- Type.get (args `nth` n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (length args)
  
  
-- | Like 'generaliseArgs' but generalises /every/ occurrence of a set of terms
-- within another.
generaliseTerms :: forall m a t . 
    ( Env.Read m, Defs.Read m, ContainsTerms t  
    , Substitutable a, Inner a ~ Term ) =>
  Set Term -> t -> (Indices.Shift -> t -> m a) -> m a
generaliseTerms (toList -> terms) target run
  | length terms == 0 = run id target
  | otherwise = do
    gen_bs <- mapM makeBind [0..length terms - 1]
          
    -- Run the inner computation
    done_t <- id
      . Env.bindMany (reverse gen_bs)
      . run liftHere 
      . mapTerms generalise
      $ target
      
    -- Reverse the generalisation
    return 
      . foldr Indices.subst done_t
      $ zipWith Indices.liftMany [0..] (reverse terms)
  where
  generalise :: Term -> Term
  generalise = id
    . concatEndos (zipWith replace terms' new_vars)
    . liftHere
    where
    terms' = map liftHere terms
    new_vars = map Var [0..length terms - 1]
    
  makeBind :: Int -> m Bind
  makeBind n
    | Var x <- terms `nth` n = Env.boundAt x
  makeBind n = do
    ty <- Type.get (terms `nth` n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (length terms)
  
  
-- | Finds uninterpreted function calls and generalises them.
generaliseUninterpreted :: ( Env.Read m, Defs.Read m, ContainsTerms t
                           , Substitutable a, Inner a ~ Term ) =>
  t -> (Indices.Shift -> t -> m a) -> m a
generaliseUninterpreted target =
  generaliseTerms f_calls target
  where
  f_calls = concatMap (collect functionCall) (containedTerms target)
    where
    functionCall (App (Var f) _) = True
    functionCall _ = False
    
  
-- | Construct an n-tuple of the given terms. Needs to read the type of the
-- terms so it can construct the appropriate n-tuple type for
-- the constructor.
tuple :: (Defs.Read m, Env.Read m) => [Term]-> m Term
tuple ts
  | length ts > 1 = do
    ind <- id
      . liftM Type.tuple
      $ mapM Type.get ts
    return (app (Con ind 0) ts)
 
equation :: (Defs.Read m, Env.Read m) => Term -> Term -> m Term
equation left right = do
  ty <- Type.get left
  let eq_ind = Type.equation ty
  return (app (Con eq_ind 0) [left, right])
  
isEquation :: Fail.Can m => Term -> m (Term, Term)
isEquation (App (Con ind 0) [left, right]) 
  | isJust (Type.isEquation ind) = return (left, right)
isEquation _ = Fail.here
          
            
-- | Take a free variable of a fixpoint and express it as a new first argument
-- of that fixpoint.
-- It can reverse the @constArg@ step from "Elea.Simplifier".
-- Necessary for then @freeDecreasingArg@ step from "Elea.Fusion".
expressFreeVariable :: Env.Read m => Index -> Term -> m Term
expressFreeVariable free_var (Fix fix_i (Bind fix_n fix_ty) fix_t) = do
  var_b <- Env.boundAt free_var
  let fix_ty' = Type.Fun (get Type.boundType var_b) fix_ty
  return
    . (\t -> app t [Var free_var])
    . Fix fix_i (Bind fix_n fix_ty')
    . Lam var_b
    . Env.trackOffset
    . Fold.transformM update
    $ Indices.lift fix_t
  where
  update :: Term -> Env.TrackOffset Term
  -- Update function calls
  update term@(App (Var f) args) = do
    idx <- Env.tracked
    if f == succ idx
    then return (app (Var (succ idx)) (Var idx : args))
    else return term
  -- Update variables occurrences
  update (Var x) = do
    idx <- Env.tracked
    if x == free_var + idx + 2
    then return (Var idx)
    else return (Var x)
  update other = 
    return other

-- | The fold of 'expressFreeVariable' over a list of indices.
-- The order of the new variables applied to the output term will match
-- the order of the indices input.
-- > expressFreeVariables [x, y, z] (fix F) = (fix G) x y z
expressFreeVariables :: forall m . Env.Read m => [Index] -> Term -> m Term
expressFreeVariables = flip (foldrM express) 
  where
  express :: Index -> Term -> m Term
  express free_var (flattenApp -> fix : args) = do
    App fix' [new_arg] <- expressFreeVariable free_var fix
    return (app fix' (new_arg : args))
    
    
-- | A /finite match/ in one in which any recursively typed variables
-- bound down any pattern branch are unused. So if a finite match over a list
-- does not reference the sub-list.
-- Could be extended to depth greater than one, viz. matching over a list
-- will only analyse a finite portion of the list.
isFiniteMatch :: Type.Ind -> [Alt] -> Bool
isFiniteMatch ind = and . zipWith recArgsUsed [0..]
  where
  recArgsUsed :: Nat -> Alt -> Bool
  recArgsUsed n (Alt _ alt_t) = 
    Set.null (Set.intersection (Indices.free alt_t) rec_args)
    where
    rec_args = Set.fromList (Type.recursiveArgIndices ind n)
    

-- | Reverting a pattern match is to replace the pattern it was matched to
-- with the term that was matched.
-- > revertMatches (match n with | 0 -> 0 | Suc x' -> Suc x' end)
-- >   = match n with | 0 -> 0 | Suc x' -> n end
-- TODO revert all matches over variables of the pattern first, otherwise
-- this function will only work for single depth matches
revertMatchesWhenM :: forall m . Env.Write m 
  -- | A predicate that will be passed terms matched upon to ask whether
  -- they should be reverted
  => (Term -> m Bool) 
  -> Term 
  -> m Term
revertMatchesWhenM when = Fold.transformM revert
  where
  revert term@(Case ind cse_t alts) = do
    here <- when cse_t
    if not here
    then return term
    else do
      let alts' = zipWith revertAlt [0..] alts
      return (Case ind cse_t alts')
    where
    revertAlt n alt
      | Type.isBaseCase ind n = alt
    revertAlt n (Alt bs alt_t) = 
      Alt bs alt_t'
      where
      cse_t' = Indices.liftMany (length bs) cse_t
      alt_t' = replace (altPattern ind n) cse_t' alt_t
  revert other = 
    return other
    
revertMatchesWhen :: (Term -> Bool) -> Term -> Term
revertMatchesWhen when = runIdentity . revertMatchesWhenM (return . when)
    
revertMatches :: Term -> Term
revertMatches = revertMatchesWhen (const True)

-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> Env.trackeds (== t))

-- | Whether the first argument is a subterm of the first
isSubterm :: Term -> Term -> Bool
isSubterm t = Env.trackIndices t . Fold.anyM (\t -> Env.trackeds (== t))
  
-- | Whether we have already attempted to fuse this set of terms into
-- this fixpoint.
alreadyFused :: FixInfo -> Set Constraint -> Term -> Bool
alreadyFused (FixInfo fused) matched fix_term = 
  any unifiable fused
  where
  unifiable :: (Set Constraint, Term) -> Bool
  unifiable (matched', fix_term')
    | Just term_uni <- Unifier.find fix_term fix_term'
    , Just m_uni <- Unifier.find matched matched' =  
      isJust (Unifier.union term_uni m_uni)
  unifiable _ = False
    
-- | Finds a context which will turn the first term into the second.
-- Basically takes the first term and replaces all instances of it 
-- with the gap in the second. Will fail if there was not at least one
-- instance.
findContext :: Fail.Can m => Term -> Term -> m Context
findContext inner full = do
  Fail.unless (inner `isSubterm` full)
  return (Context.make (\gap -> replace inner gap full))
  
