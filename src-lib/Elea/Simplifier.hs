-- | This module performs term simplifications which do 
-- not involve fixpoint fusion.
module Elea.Simplifier
(
  run, runChecked,
  steps, removeConstArgs,
  
  -- | These two steps are useful for fixpoint fusion
  finiteCaseFix, finiteArgFix,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Evaluation as Eval
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans

run :: Term -> Term
run = Fold.rewriteSteps steps

runChecked :: (Env.Read m, Defs.Read m) => Term -> m Term
runChecked = Fold.rewriteStepsM (map Type.checkStep steps)

steps :: Fail.Can m => [Term -> m Term]
steps = Eval.steps ++
  [ const Fail.here
  , Fail.concatTransforms transformSteps
  , finiteArgFix
  , unfoldFixInj
  , freeCaseFix
  , propagateMatch
  , raiseVarCase
  , finiteCaseFix 
  , unfoldWithinFix
  ]
  where
  -- Transformation steps do not require sub terms to be rewritten upon success.
  transformSteps = 
    [ const Fail.here
    , caseFun
    , constantFix
    , absurdity
    , constArg
    , identityCase
    , uselessFix
    ] 

-- | Remove arguments to a fixpoint if they never change in 
-- any recursive calls.
removeConstArgs :: Term -> Term
removeConstArgs = Eval.run . Fold.rewrite constArg

  
-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: Fail.Can m => Term -> m Term
caseFun cse@(Case ind t alts) = do
  Fail.unless (length potential_bs > 0)
  return
    . Lam (head potential_bs)
    . Case ind (Indices.lift t)
    $ map appAlt alts
  where
  alt_ts = map (get altInner) alts
  
  potential_bs = mapMaybe findLam alts
    where
    findLam :: Alt -> Maybe Bind
    findLam (Alt _ (Lam b _)) = Just b
    findLam _ = Nothing
  
  appAlt (Alt bs alt_t) =
    Alt bs (app alt_t' [arg])
    where
    alt_t' = Indices.liftAt (toEnum (length bs)) alt_t
    arg = Var (toEnum (length bs))
    
caseFun _ = Fail.here


-- | Finds terms that are absurd and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
absurdity :: Fail.Can m => Term -> m Term
absurdity (App (Absurd ty) args) = 
  return (Absurd new_ty)
  where
  new_ty = id
    . Type.unflatten 
    . drop (length args) 
    $ Type.flatten ty
absurdity _ =
  Fail.here
  
  
-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
constArg :: Fail.Can m => Term -> m Term
constArg term@(Fix fix_info (Bind fix_name fix_ty) fix_t) = do
  -- Find if any arguments never change in any recursive calls
  pos <- Fail.fromMaybe (find isConstArg [0..length arg_bs - 1])
  
  -- Then we run the 'removeConstArg' function on that position
  return (Eval.run (removeConstArg pos))
  where
  -- Strip off the preceding lambdas of the function
  (arg_bs, fix_body) = flattenLam fix_t
  arg_count = length (Type.flatten fix_ty) - 1
  
  -- The index of the recursive call to the function within 'fix_body'
  fix_f = length arg_bs :: Index
  
  -- Does the given argument position never change in any recursive call?
  isConstArg :: Int -> Bool
  isConstArg arg_i = id
    . not
    . Env.trackIndices (fix_f, Var arg_x)
    $ Fold.anyM isntConst fix_body
    where
    -- The index of the argument we are tracking as it was bound
    -- by the lambdas of the function
    arg_x = enum (length arg_bs - (arg_i + 1))
    
    -- Whether this given argument changes at a recursive call site
    isntConst :: Term -> Env.TrackIndices (Index, Term) Bool
    isntConst (App (Var f) args) = do
      (fix_f, arg_t) <- Env.tracked
      return 
        $ fix_f == f
        -- If the number of arguments differs then this function is not
        -- in the correct shape for this process. So we fail by saying
        -- the argument changed.
        && (length args /= arg_count
        || arg_t /= (args !! arg_i))
    isntConst _ = 
      return False
     
      
  -- Remove an argument to the function at the given position.
  removeConstArg :: Int -> Term
  removeConstArg arg_i = id
    -- Add new outer lambdas to keep the type of the term the same
    . unflattenLam (left_bs ++ [dropped_b])
    . flip app outer_args
    
    -- Need to make sure no variables are captured by these new outer lambdas
    . Indices.liftManyAt (length left_bs) 1 
    
    -- The fixpoint information is lifted by one to take into account the 
    -- removed argument index (which is 0 at this point).
    . Fix (Indices.lift fix_info) fix_b'
    
    -- Remove the argument everywhere it appears
    . Env.trackIndices 0
    . Fold.transformM removeArg
    
    -- Remove the lambda, and replace all occurrences of that variable
    -- with index 1 (here index 0 will be the fix variable)
    . Indices.substAt Indices.omega (Var 1)
    . Indices.liftAt 1
    . unflattenLam left_bs
    . Indices.substAt 0 (Var Indices.omega)
    . unflattenLam right_bs
    $ fix_body
    where
    -- Lambdas to the left and right of the removed lambda
    (left_bs, dropped_b:right_bs) = splitAt arg_i arg_bs
    
    -- The arguments that will be applied outside the fix
    outer_args = id
      . map (Var . enum)
      $ reverse [1..arg_i]
    
    -- The new type binding for the fix, with the given argument removed
    fix_b' = Bind fix_name fix_ty'
      where
      fix_ty' = id
        . Type.unflatten
        . removeAt arg_i
        $ Type.flatten fix_ty
    
    removeArg :: Term -> Env.TrackIndices Index Term
    removeArg term@(App (Var f) args) = do   
      fix_f <- Env.tracked
      if fix_f == f
      then return (App (Var f) (removeAt arg_i args))
      else return term
    removeArg term = 
      return term
      
constArg _ = Fail.here


-- | If a fixpoint has a finite input to one of its decreasing arguments
-- then you can safely unfold it.
finiteArgFix :: Fail.Can m => Term -> m Term
finiteArgFix term@(App fix@(Fix {}) args)
  | any isFinite dec_args = 
    return (Eval.run (app (Term.unfoldFix fix) args))
  where
  dec_args = map (args !!) (Term.decreasingAppArgs term)
finiteArgFix _ = Fail.here


-- | Removes a pattern match which just returns the term it is matching upon.
identityCase :: Fail.Can m => Term -> m Term
identityCase (Case ind cse_t alts)
  | and (zipWith isIdAlt [0..] alts) = return cse_t
  where
  isIdAlt :: Nat -> Alt -> Bool
  isIdAlt n (Alt _ alt_t) = alt_t == altPattern ind n
identityCase _ = Fail.here


-- | Dunno if this ever comes up but if we have a fix without any occurrence
-- of the fix variable in the body we can just drop it.
uselessFix :: Fail.Can m => Term -> m Term
uselessFix (Fix _ _ fix_t)
  | not (0 `Set.member` Indices.free fix_t) = 
    return (Indices.lower fix_t)
uselessFix _ = Fail.here


-- | Unfolds a 'Fix' if one of its arguments is a constructor term.
unfoldFixInj :: Fail.Can m => Term -> m Term
unfoldFixInj term@(App fix@(Fix _ _ fix_t) args)
  | any isSafeConArg (Term.decreasingAppArgs term) = 
    return (Eval.run (app (Term.unfoldFix fix) args))
  where
  -- Check whether an argument is a constructor, and does not unify
  -- with any recursive calls the function itself makes.
  isSafeConArg :: Int -> Bool
  isSafeConArg arg_i =
    isCon (leftmost arg) && not any_unify
    where
    arg = args !! arg_i
    
    any_unify = id
      . Env.trackIndices 0
      $ Fold.anyM unifies fix_t
      where
      unifies :: Term -> Env.TrackIndices Index Bool
      unifies (App (Var f) args') = do
        f_var <- Env.tracked
        return 
           $ f == f_var 
          && length args' > arg_i 
          && uni_exists
        where
        arg' = args' !! arg_i
        uni_exists = Unifier.exists arg arg'
      unifies _ =
        return False
        
unfoldFixInj _ = Fail.here


-- | If we pattern match inside a 'Fix', but only using variables that exist
-- outside of the 'Fix', then we can float this pattern match outside
-- of the 'Fix'.
freeCaseFix :: Fail.Can m => Term -> m Term
freeCaseFix fix@(Fix _ _ fix_t) = do
  free_case <- id
    . Fail.fromMaybe
    . Env.trackOffset
    . Env.liftTracked
    $ Fold.findM freeCases fix_t
  return (Term.applyCase free_case fix)
  where
  freeCases :: Term -> Env.TrackOffset (Maybe Term)
  freeCases cse@(Case _ cse_t _) = do
    idx_offset <- Env.tracked
    if any (< idx_offset) (Indices.free cse_t) 
    then return Nothing
    else return 
       . Just
       . Indices.lowerMany (enum idx_offset) 
       $ cse
  freeCases _ = 
    return Nothing
freeCaseFix _ = Fail.here


-- | Replace all instances of a term with the pattern it is matched to.
propagateMatch :: Fail.Can m => Term -> m Term
propagateMatch (Case ind cse_t alts) 
  | any altSubterm alts =
    return (Case ind cse_t (zipWith propagateAlt [0..] alts))
  where
  altSubterm (Alt bs alt_t) = 
    Term.isSubterm (Indices.liftMany (length bs) cse_t) alt_t
  
  propagateAlt :: Nat -> Alt -> Alt
  propagateAlt n (Alt bs alt_t) = id
    . Alt bs  
    . Eval.run
    $ Term.replace cse_t' alt_p alt_t
    where
    alt_p = Term.altPattern ind n
    cse_t' = Indices.liftMany (length bs) cse_t
    
propagateMatch _ = Fail.here


-- | Pattern matches over variables should be above those over function
-- results.
raiseVarCase :: forall m . Fail.Can m => Term -> m Term
raiseVarCase outer_t@(Case _ (leftmost -> Fix {}) alts) = do
  inner_case <- Fail.choose (map caseOfVarAlt alts)
  return (Term.applyCase inner_case outer_t)
  where
  caseOfVarAlt :: Alt -> m Term
  -- We return the inner alt case-of if it is over a variable
  -- which is not from the pattern match, viz. it can be lowered
  -- to outside the match.
  caseOfVarAlt (Alt bs alt_t@(Case ind (Var x) i_alts)) = do
    x' <- Indices.tryLowerMany (length bs) x
    return (Case ind (Var x') i_alts)
  caseOfVarAlt _ = 
    Fail.here
    
raiseVarCase _ = Fail.here


-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
-- Currently only works for a single unrolling, but otherwise we'd need an 
-- arbitrary amount of unrolling, which seems difficult.
finiteCaseFix :: Fail.Can m => Term -> m Term
finiteCaseFix term@(Case ind (App (Fix _ _ fix_t) args) alts) = do
  -- This line is just for speed.
  -- It's just a guess, but I can't think of a non-recursive datatype
  -- returning function which this would work on.
  Fail.unless (Type.isRecursive ind)
  
  -- Check that unrolling the function removed recursive calls 
  Fail.when (0 `Indices.freeWithin` unrolled)
  return (Indices.lower unrolled)
  where
  extendedEval = Fold.rewriteSteps (Eval.steps ++ [finiteCaseFix])
  unrolled = id
    . extendedEval
    . Case ind (App fix_t (Indices.lift args)) 
    $ Indices.lift alts
  
finiteCaseFix _ = Fail.here


-- | If a recursive function just returns the same value, regardless of its
-- inputs, just reduce it to that value.
constantFix :: forall m . Fail.Can m => Term -> m Term
constantFix (Fix _ fix_b fix_t)
  | Just [result] <- mby_results = guessConstant result
  | Just [] <- mby_results = guessConstant (Absurd result_ty)
  where
  (arg_bs, _) = flattenLam fix_t
  result_ty = Type.returnType (get Type.boundType fix_b)
  mby_results = potentialResults fix_t

  potentialResults :: Term -> Maybe [Term]
  potentialResults = id
    . map toList
    . Env.trackIndices 0
    . runMaybeT
    . Fold.isoFoldM Term.branches resultTerm
    where
    resultTerm :: Term -> MaybeT (Env.TrackIndices Index) (Set Term)
    resultTerm (Absurd _) = return mempty
    resultTerm term = do
      fix_f <- Env.tracked
      if leftmost term == Var fix_f
      then return mempty
      else do
        let depth = enum fix_f
        -- Checking we can lower by one extra makes sure there is no
        -- occurrence of the 'fix'ed variable itself.
        guard (Indices.lowerableBy (depth + 1) term)
        return
          . Set.singleton
          $ Indices.lowerMany depth term
        
  guessConstant :: Term -> m Term
  guessConstant guess_t 
    | Just [] <- mby_results' = return const_t
    | Just [guess_t'] <- mby_results' = id
      . assert (guess_t == guess_t') 
      $ return const_t
    where
    rec_f = id
      . unflattenLam arg_bs
      . Indices.liftMany (length arg_bs)
      $ guess_t
      
    fix_t' = Eval.run (Term.replace (Var 0) rec_f fix_t)
    mby_results' = potentialResults fix_t'
    const_t = Indices.lower rec_f
    
  guessConstant _ = 
    Fail.here
        
constantFix _ = 
  Fail.here
  
  
-- | Unfolds a 'Fix' within itself if it can be unrolled at
-- at a point it is called recursively.
unfoldWithinFix :: Fail.Can m => Term -> m Term
unfoldWithinFix fix@(Fix fix_i fix_b fix_t) = do
  Fail.unless any_replaced
  return (Fix fix_i fix_b fix_t')
  where
  (fix_t', Monoid.Any any_replaced) = id
    . Env.trackIndices (0, fix_t)
    . runWriterT
    $ Fold.transformM unfold fix_t
  
  unfold :: Term -> WriterT Monoid.Any (Env.TrackIndices (Index, Term)) Term
  unfold term@(App (Var f) args) = do
    (fix_f, fix_t) <- Env.tracked
    if f /= fix_f || any (not . Term.isFinite) args
    then return term
    else do
      tell (Any True)
      (_, fix_t) <- Env.tracked
      return (Eval.run (app fix_t args))
  unfold other = 
    return other
  
unfoldWithinFix _ = Fail.here
