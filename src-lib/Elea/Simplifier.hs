-- | This module performs term simplifications which do 
-- not involve fixpoint fusion.
module Elea.Simplifier
(
  run, steps, removeConstArgs
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Evaluation as Eval
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans

run :: (Env.Read m, Defs.Read m) => Term -> m Term
run = Fold.rewriteStepsM (map Type.checkStep steps)

steps :: (Fail.Can m, Env.Read m, Defs.Read m) => [Term -> m Term]
steps = eval_steps ++
  [ const Fail.here
  , caseFun
  , caseApp
  , appCase
  , caseCase
  , absurdity
  , constArg
  , identityCase
  , uselessFix
  , finiteArgFix
  , unfoldFixInj
  , freeCaseFix
  , propagateVarMatch
  , raiseVarCase
  , constantCase
  ]
  where
  eval_steps = map (Fail.fromMaybe .) Eval.steps
  
  
-- | Remove arguments to a fixpoint if they never change in 
-- any recursive calls.
removeConstArgs :: Env.Write m => Term -> m Term
removeConstArgs = Fold.rewriteM constArg

  
-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: (Fail.Can m, Env.Read m, Defs.Read m) => Term -> m Term
caseFun cse@(Case ind t alts) = do
  cse_ty <- Type.get cse
  -- We only apply this step if the pattern match is of function type.
  Fail.unless (Type.isFun cse_ty)
  let Type.Fun arg_ty _ = cse_ty
  return
    . Lam (Bind "X" arg_ty)
    . Case ind (Indices.lift t)
    $ map appAlt alts
  where
  alt_ts = map (get altInner) alts
  
  appAlt (Alt bs alt_t) =
    Alt bs (app alt_t' [arg])
    where
    alt_t' = Indices.liftAt (toEnum (length bs)) alt_t
    arg = Var (toEnum (length bs))
    
caseFun _ = Fail.here


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Fail.Can m => Term -> m Term
caseApp (App (Case ind t alts) args) =
  return (Case ind t (map appArg alts))
  where
  appArg (Alt bs alt_t) =
    Alt bs (app alt_t (Indices.liftMany (nlength bs) args))
    
caseApp _ = Fail.here


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
appCase :: Fail.Can m => Term -> m Term
appCase term@(App _ args) = do
  cse_t <- Fail.fromMaybe (find isCase args)
  return (Term.applyCase cse_t term)
  
appCase _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Fail.Can m => Term -> m Term
caseCase outer_cse@(Case _ inner_cse@(Case {}) _) =
  return (Term.applyCase inner_cse outer_cse)
caseCase _ = Fail.here


-- | Finds terms that are absurd and sets them that way.
-- So far it detects pattern matching over absurdity, 
-- and applying arguments to an absurd function.
absurdity :: (Fail.Can m, Env.Read m, Defs.Read m) => Term -> m Term
absurdity term
  | isAbsurd term = do
    ty <- Type.get term
    return (Absurd ty)
  where
  isAbsurd (App (Absurd _) _) = True
  isAbsurd (Case _ (Absurd _) _) = True
  isAbsurd _ = False
    
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
  
  -- The index of the recursive call to the function within 'fix_body'
  fix_f = enum (length arg_bs) 
  
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
        && arg_t /= (args !! arg_i)
    isntConst _ = 
      return False
     
      
  -- Remove an argument to the function at the given position.
  removeConstArg :: Int -> Term
  removeConstArg arg_i = id
    -- Add new outer lambdas to keep the type of the term the same
    . unflattenLam (left_bs ++ [dropped_b])
    . flip app outer_args
    
    -- Need to make sure no variables are captured by these new outer lambdas
    . Indices.liftManyAt (elength left_bs) 1 
    
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
finiteArgFix (App fix@(Fix {}) args)
  | any isFinite dec_args = 
    return (App (Term.unfoldFix fix) args)
  where
  dec_args = map (args !!) (Term.decreasingArgs fix)
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
unfoldFixInj term@(App fix@(Fix {}) args)
  | any isConArg (Term.decreasingArgs fix) = 
    return (App (Term.unfoldFix fix) args)
  where
  -- Check whether an argument is a constructor, and does not unify
  -- with any recursive calls the function itself makes (TODO).
  isConArg :: Int -> Bool
  isConArg arg_i = isCon . leftmost $ args !! arg_i
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


-- | If we pattern match over a variable, we replace all instances of 
-- that variable with the matched pattern down each branch.
propagateVarMatch :: Fail.Can m => Term -> m Term
propagateVarMatch (Case ind var@(Var idx) alts) 
  | idx `Set.member` concatMap Indices.free alts =
    return (Case ind var (zipWith propagateAlt [0..] alts))
  where
  propagateAlt :: Nat -> Alt -> Alt
  propagateAlt n (Alt bs alt_t) =
    Alt bs (Indices.replaceAt idx' alt_p alt_t)
    where
    alt_p = Term.altPattern ind n
    idx' = Indices.liftMany (elength bs) idx
    
propagateVarMatch _ = Fail.here


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
    x' <- Indices.tryLowerMany (elength bs) x
    return (Case ind (Var x') i_alts)
  caseOfVarAlt _ = 
    Fail.here
    
raiseVarCase _ = Fail.here


-- | Removes a pattern match if every branch returns the same value.
constantCase :: forall m . Fail.Can m => Term -> m Term
constantCase (Case _ _ alts) = do
  (alt_t:alt_ts) <- mapM loweredAltTerm alts
  Fail.unless (all (== alt_t) alt_ts)
  return alt_t
  where
  loweredAltTerm :: Alt -> m Term
  loweredAltTerm (Alt bs alt_t) = 
    Indices.tryLowerMany (elength bs) alt_t
constantCase _ = Fail.here


