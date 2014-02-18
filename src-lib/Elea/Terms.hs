-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  module Elea.Term,
  branches,
  replace,
  unfoldFix,
  collectM, collect,
  decreasingArgs,
  decreasingAppArgs,
  strictVars,
  applyCase,
  generaliseArgs,
  pair,
  equation,
  isConstraint,
  removeConstraints,
  constraint,
  isProductive,
  isFiniteMatch,
  revertMatchesWhenM, 
  revertMatchesWhen, 
  revertMatches,
  occurrences,
  isSubterm,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
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
  
  
-- | Collect terms which fulfil a given predicate. 
-- The variables of these terms must be free outside the original term,
-- and will be automatically lowered to the correct indices.
collectM :: forall m . Env.Write m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    c <- (Trans.lift . Trans.lift . p) t
    guard c
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
      else Env.isSmaller (args !! arg_i)
    decreasing _ = 
      return True
      
-- | The variables whose value must be known in order to unroll the given term.
-- If we expanded this term these variables would be inside pattern matches
-- topmost.
strictVars :: Term -> Set Index
strictVars (App (Fix _ _ fix_t) args) = id
  . Set.intersection (Indices.free args)
  . Env.trackIndices 0
  . Fold.foldM matchedUpon
  . Eval.run
  $ App fix_t args
  where
  matchedUpon :: Term -> Env.TrackIndices Index (Set Index)
  matchedUpon (Case _ (Var x) _) = do
    offset <- Env.tracked
    if x >= offset
    then return (Set.singleton (x - offset))
    else return mempty
  matchedUpon _ = 
    return mempty
  
      
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
generaliseArgs :: (Indexed a, Substitutable t, 
                   Inner t ~ Term, Env.Read m, Defs.Read m) =>
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
    . zipWith Indices.liftMany [0..]
    $ reverse args
  where
  new_vars = map Var [0..length args - 1]
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (length args)
  
  
-- | Whether a term is a constraint. 
isConstraint :: Term -> Bool
isConstraint (Case _ _ alts)
  | length alts > 1
  , [alt_i] <- findIndices (not . isAbsurd . get altInner) alts =
    lowerableAltInner (alts !! alt_i)
isConstraint _ = False

  
-- | Removes any instances of constraints
removeConstraints :: Term -> Term
removeConstraints = Fold.transform remove
  where
  remove (Case _ _ alts)
    -- Find the single non-absurd branch, and return just that term
    | length alts > 1
    , [alt_i] <- findIndices (not . isAbsurd . get altInner) alts
    , lowerableAltInner (alts !! alt_i) =
      loweredAltInner (alts !! alt_i)
    where
  remove term = term
        
  
-- | Construct a pair of the two given terms. Needs to read the type of the
-- two terms so it can construct the appropriate cartesian product type for
-- the constructor.
pair :: (Defs.Read m, Env.Read m) => Term -> Term -> m Term
pair left right = do
  left_ty <- Type.get left
  right_ty <- Type.get right
  let pair_ind = Type.pair left_ty right_ty
  return (app (Con pair_ind 0) [left, right])
 
equation :: (Defs.Read m, Env.Read m) => Term -> Term -> m Term
equation left right = do
  ty <- Type.get left
  let eq_ind = Type.equation ty
  return (app (Con eq_ind 0) [left, right])
  
      
-- | Create a constraint context. For example:
-- > constraint (reverse xs) 0 nat == assert Nil <- reverse xs in _
-- > constraint (reverse xs) 1 nat == assert Cons y ys <- reverse xs in _
constraint :: ()
  => Term -- ^ The term we constrain to be a specific constructor
  -> Type.Ind  -- ^ The inductive type of the first argument
  -> Nat  -- ^ The constructor index we are constraining this term to be
  -> Type -- ^ The type of the gap in the context
  -> Context 
constraint match_t ind con_n result_ty =
  Context.make makeContext
  where
  makeContext gap_t = 
    Case ind match_t alts
    where
    cons = Type.unfold ind
    alts = map buildAlt [0..length cons - 1]
    
    buildAlt :: Int -> Alt
    buildAlt alt_n = 
      Alt bs alt_t
      where
      Bind _ con_ty = id
        . assert (length cons > alt_n)
        $ cons !! alt_n
      bs = map (Bind "X") (init (Type.flatten con_ty))
      
      -- If we are not down the matched branch we return absurd
      alt_t | enum alt_n /= con_n = Absurd result_ty
            | otherwise = gap_t
            
            {-
-- | Take a free variable of a fixpoint and express it as a new first argument
-- of that fixpoint.
-- It can reverse the @constArg@ step from "Elea.Simplifier".
-- Necessary for then @freeDecreasingArg@ step from "Elea.Fusion".
expressFreeVariable :: Index -> Term -> Term
expressFreeVariable free_var (Fix fix_i fix_b fix_t) =
  
  where
  replaceRecCall :: Term -> Env.TrackIndices Index Term
  replaceRecCall term@(App (Var f) args) = do
    old_f <- Env.tracked
    if f /= old_f
    then return term
    else return (app (Var (succ old_f)) (Var old_f : args))
            -}

-- | A function is /productive/ if every recursive call is guarded
-- by a constructor.
isProductive :: Term -> Bool
isProductive (Fix _ _ fix_t) = id
  -- Track the index of the recursive call to the fixpoint
  . Env.trackIndices 0
  $ Fold.isoAllM branches productive fix_t
  where
  -- A productive branch is either in HNF, or returns a term with
  -- no recursive call to the fixpoint inside it.
  productive :: Term -> Env.TrackIndices Index Bool
  productive (leftmost -> Con {}) = 
    return True
  productive term = do
    fix_f <- Env.tracked
    return (not (fix_f `Indices.freeWithin` term))
    
    
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
