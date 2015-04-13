-- | An extension of 'Elea.Transform.Evaluate'. Slightly more heavyweight
-- transformations but fundamentally those which make terms smaller and simpler.
-- They do not require fusion and they do not need to make use of 'Elea.Embed'.
module Elea.Transform.Simplify
(
  run, 
  quick,
  steps,
  --removeConstArgs,
  willUnfold,
  
  -- | These two steps are useful for fixpoint fusion
  --finiteCaseFix, 
  finiteArgFix,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Terms as Term
import qualified Elea.Term.Height as Height
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Transform as Transform
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans

run :: Term -> Term
run = flip runReader ([] :: [Bind]) 
    -- ^ Use the Reader [Bind] instance for type environments
    . Transform.runStep (Fail.concatTransforms all_steps)
    . Term.reset
  where
  all_steps = Eval.transformSteps ++ steps ++ Eval.traverseSteps

quick :: Term -> Term
quick = run

steps :: Transform.Step m => [Term -> m Term]
steps =
    [ const Fail.here
    , Height.assertDecrease "case-app" caseApp
    , Height.assertDecrease "fun-case" appCase
    , Height.assertDecrease "case-case" caseCase
    , Height.assertDecrease "case-fun" caseFun
    , Height.assertDecrease "const-arg" constArg
    , Height.assertDecrease "id-case" identityCase
    , Height.assertDecrease "const-case" constantCase
    , Height.assertDecrease "const-fix" constantFix
    , Height.assertDecrease "unfold-finite" unfoldFinite
    {-
    , uselessFix
    , propagateMatch
    , finiteArgFix
    , unfoldFixInj
    , freeCaseFix   
    , unfoldWithinFix
   -- , finiteCaseFix
    , unsafeUnfoldFixInj
    -}
    ] 
    

  


-- | Whether a fixpoint call will be unfolded by the simplifier
willUnfold :: Term -> Bool
willUnfold term@(App fix@(Fix _ _ fix_t) args) = 
  any isSafeConArg (Term.decreasingAppArgs term)
  where
  -- Check whether an argument is a constructor, and will reduce all pattern 
  -- matches upon it.
  isSafeConArg :: Int -> Bool
  isSafeConArg arg_i =
    isCon (leftmost arg) 
    && Set.null matched_rec_vars
    where
    arg = args !! arg_i
    -- All the recursive arguments to the constructor
    -- at the given argument position, and all the recursive arguments to
    -- those arguments, and so on...
    rec_vars = recursiveConArgs arg
    
    matched_vars = id
      . Env.trackOffset
      . Env.liftTracked
      $ Fold.foldM matchedVars unwrapped_t
      
    matched_rec_vars = Set.intersection matched_vars rec_vars
    
    unwrapped_t = id
      . Eval.run 
      . App fix_t 
      . setAt arg_i (Indices.lift arg)
      $ replicate (length args) (Var 0)
      
    recursiveConArgs :: Term -> Set Index
    recursiveConArgs (Var x) = Set.singleton x
    recursiveConArgs (flattenApp -> Con con : con_args) = id
      . concatMap recursiveConArgs
      . map (con_args !!)
      $ Type.recursiveArgs con
    recursiveConArgs _ = mempty
    
    -- Free variables pattern matched upon in the given term
    matchedVars :: Term -> Env.TrackOffset (Set Index) 
    matchedVars (Case (Var x) alts) = do
      can_lower <- Env.lowerableByOffset x
      -- Variable must be free outside the original term,
      -- and must be recursed upon (so the match cannot be finite)
      if can_lower 
        && not (Term.isFiniteMatch alts)
      then liftM Set.singleton (Env.lowerByOffset x)
      else return mempty
    matchedVars _ = 
      return mempty

  
-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: Transform.Step m => Term -> m Term
caseFun cse@(Case t alts) 
  | Just new_b <- potentialBinds alts = do
    Transform.continue
      . Lam new_b
      . Case (Indices.lift t)
      $ map appAlt alts
  where
  alt_ts = map (get altInner) alts
  
  potentialBinds :: [Alt] -> Maybe Bind
  potentialBinds = 
    msum . map findLam
    where
    findLam :: Alt -> Maybe Bind
    findLam (Alt _ _ (Lam b _)) = Just b
    findLam (Alt _ _ (Case _ alts)) = potentialBinds alts
    findLam _ = Nothing
  
  appAlt (Alt con bs alt_t) =
    Alt con bs (Eval.run (app alt_t' [arg]))
    where
    alt_t' = Indices.liftAt (length bs) alt_t
    arg = Var (length bs)
    
caseFun _ = Fail.here
  
  
-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
constArg :: Transform.Step m => Term -> m Term
constArg (App fix@(Fix fix_info (Bind fix_name fix_ty) fix_t) args)
  | length arg_bs /= nlength args = Fail.here
  | otherwise = do
    -- Find if any arguments never change in any recursive calls
    pos <- Fail.fromMaybe (find isConstArg [0..length arg_bs - 1])
    
    -- Then we run the 'removeConstArg' function on that position to
    -- get a new fixed-point
    let fix' = removeConstArg pos
    
    -- Might as well simplify the constant argument before pushing it inside
    -- and possible duplicating it
    arg' <- Transform.continue (args !! pos)
    let args' = setAt pos arg' args
    
    -- Run evaluation to reduce all the new lambdas
    -- then continue with simplification
    Transform.continue (Eval.run (app fix' args'))
  where
  -- Strip off the preceding lambdas of the function
  (arg_bs, fix_body) = flattenLam fix_t
  arg_count = nlength (Type.flatten fix_ty) - 1
  
  -- The index of the recursive call to the function within 'fix_body'
  fix_f = length arg_bs :: Index
  
  -- Does the given argument position never change in any recursive call?
  isConstArg :: Int -> Bool
--  isConstArg arg_i
 --   | isVar (args !! arg_i) = False
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
        || arg_t /= (args `nth` arg_i))
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
    . Fix fix_info' fix_b'
    
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
    -- The fixed-point will no longer be closed since we have
    -- moved the constant argument outside it
    fix_info' = set fixClosed False fix_info
    
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
  dec_args = map (args `nth`) (Term.decreasingAppArgs term)
finiteArgFix _ = Fail.here


-- | Removes a pattern match which just returns the term it is matching upon.
identityCase :: Transform.Step m => Term -> m Term
identityCase (Case cse_t alts)
  | all isIdAlt alts = Transform.continue cse_t
  where
  isIdAlt :: Alt -> Bool
  isIdAlt (Alt con _ alt_t) = 
    alt_t == altPattern con
identityCase _ = Fail.here


-- | Dunno if this ever comes up but if we have a fix without any occurrence
-- of the fix variable in the body we can just drop it.
uselessFix :: Fail.Can m => Term -> m Term
uselessFix (Fix _ _ fix_t)
  | not (0 `Set.member` Indices.free fix_t) = 
    return (Indices.lower fix_t)
uselessFix _ = Fail.here

{-
-- | Unfolds a 'Fix' if one of its decreasing arguments is a constructor term 
-- which is defined enough to fully reduce all matches on that argument.
unfoldFixInj :: Fail.Can m => Term -> m Term
unfoldFixInj term@(App fix@(Fix _ _ fix_t) args)
  | willUnfold term =
    return   
    --  . traceMe "[unfold fix-inj] after"
      . Eval.run 
    --  . traceMe "[unfold fix-inj] before"
      $ app (Term.unfoldFix fix) args
  
unfoldFixInj _ = Fail.here
-}


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
  freeCases cse@(Case cse_t _) = do
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
propagateMatch (Case cse_t alts) 
  | any altSubterm alts = id
    return (Case cse_t (map propagateAlt alts))
  where
  altSubterm (Alt _ bs alt_t) = 
    Term.isSubterm (Indices.liftMany (length bs) cse_t) alt_t
  
  propagateAlt :: Alt -> Alt
  propagateAlt (Alt con bs alt_t) = id
    . Alt con bs  
    . Eval.run
    $ Term.replace cse_t' alt_p alt_t
    where
    alt_p = Term.altPattern con
    cse_t' = Indices.liftMany (length bs) cse_t
    
propagateMatch _ = Fail.here

{-
-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
-- Currently only works for a single unrolling, but otherwise we'd need an 
-- arbitrary amount of unrolling, which seems difficult.
finiteCaseFix :: Fail.Can m => Term -> m Term
finiteCaseFix term@(Case (App fix@(Fix _ _ fix_t) args) alts) = do
  -- I don't think this will ever apply to non-recursive data types...
  Fail.when (all Type.isBaseCase (map (get altConstructor) alts))
  Fail.unless (all finiteAlt alts)
  
  -- Check that unrolling the function removed recursive calls 
  Fail.when (0 `Indices.freeWithin` simp_t)
 
  return  
   -- . traceMe "[finite case-fix] after"
    . extendedEval
   -- . traceMe "[finite case-fix] before"
    $ Case (App (Term.unfoldFix fix) args) alts
  where
  simp_t = id
    . extendedEval
    . Case (App fix_t (Indices.lift args))
    $ map simplifyAlt alts
    
  extendedEval :: Term -> Term
  extendedEval = 
    Fold.rewriteSteps (Transform.steps ++ [constantCase, finiteCaseFix])
    
  -- A branch in which a recursive pattern variable is used
  finiteAlt :: Alt -> Bool
  finiteAlt (Alt con bs alt_t) =
    Set.null (Indices.free alt_t `Set.intersection` rec_vars)
    where
    rec_vars = Set.fromList (Type.recursiveArgIndices con)
    
  simplifyAlt :: Alt -> Alt
  simplifyAlt (Alt con bs alt_t) =
    Alt con bs (app (Con con) p_args)
    where
    free_vars = Indices.free alt_t
    
    p_args = id
      . map removeUnused
      . Term.arguments 
      $ Term.altPattern con
      where
      removeUnused (Var x)
        | Set.member x free_vars = Var x
        | otherwise = Var Indices.omega
  
finiteCaseFix _ = Fail.here
-}

-- | If a recursive function just returns the same value, regardless of its
-- inputs, just reduce it to that value.
constantFix :: forall m . Fail.Can m => Term -> m Term
constantFix t@(App (Fix _ fix_b fix_t) args)
  | length args /= nlength arg_bs = Fail.here
   
  | Just [result] <- mby_results
  , correctGuess result = return result
  
  | Just [] <- mby_results
  , correctGuess (Unr result_ty) = return (Unr result_ty)
  
  where
  (arg_bs, _) = flattenLam fix_t
  fix_ty = get Type.bindType fix_b
  result_ty = Type.returnType fix_ty
  
  mby_results = id
    . potentialResults
    $ Indices.substAt 0 (Unr fix_ty) fix_t
  
  potentialResults :: Term -> Maybe [Term]
  potentialResults = id
    . map toList
    . Env.trackOffset
    . runMaybeT
    . Fold.isoFoldM Term.branches resultTerm
    where
    resultTerm :: Term -> MaybeT Env.TrackOffset (Set Term)
    resultTerm (Eval.run -> term)
      | isUnr term = return mempty
      | otherwise = do
        depth <- Env.offset
        Fail.unless (Indices.lowerableBy depth term)
        return
          . Set.singleton
          $ Indices.lowerMany depth term
      
  correctGuess :: Term -> Bool
  correctGuess guess_t
    | Just [] <- mby_results' = True
    | Just [guess_t'] <- mby_results' = id
      . assert (guess_t == guess_t') 
      $ True
    | otherwise = False
    where
    rec_f = id
      . unflattenLam arg_bs
      . Indices.liftMany (length arg_bs)
      $ guess_t
      
    fix_t' = Eval.run (Indices.substAt 0 rec_f fix_t)
    mby_results' = potentialResults fix_t'
        
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
          

-- | Removes a pattern match if every branch returns the same value.
constantCase :: forall m . Transform.Step m => Term -> m Term
constantCase (Case _ alts) = do
  (alt_t:alt_ts) <- mapM loweredAltTerm alts
  Fail.unless (all (== alt_t) alt_ts)
  Transform.continue alt_t
  where
  loweredAltTerm :: Alt -> m Term
  loweredAltTerm (Alt _ bs alt_t) = 
    Indices.tryLowerMany (length bs) alt_t
    
constantCase _ = Fail.here



-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Transform.Step m => Term -> m Term
caseApp (App (Case t alts) args) =
  Transform.continue (Case t (map appArg alts))
  where
  appArg (Alt con bs alt_t) =
    Alt con bs (app alt_t (Indices.liftMany (length bs) args))
    
caseApp _ = Fail.here


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
appCase :: Transform.Step m => Term -> m Term
appCase term@(App _ args) = do
  cse_t <- Fail.fromMaybe (find isCase args)
  Transform.continue (Term.applyCase cse_t term)
appCase _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Transform.Step m => Term -> m Term
caseCase outer_cse@(Case inner_cse@(Case inner_t inner_alts) outer_alts) =
  Transform.continue (Case inner_t (map newOuterAlt inner_alts))
  where
  newOuterAlt :: Alt -> Alt
  newOuterAlt (Alt con bs t) = 
    Alt con bs (Case t alts_here)
    where
    alts_here = map (Indices.liftMany (length bs)) outer_alts
caseCase _ = Fail.here

{-
-- | Need to unroll fixpoints where a recursive argument is a uninterpreted
-- function call. Useful sometimes for fixfix fusion, but can cause loops
-- if you give it weird functions.
unsafeUnfoldFixInj :: Fail.Can m => Term -> m Term
unsafeUnfoldFixInj (App fix@(Fix {}) args)
  | any unfoldable args = id
    . return
    $ app (Term.unfoldFix fix) args
  where 
  rec_args = map (args !!) (Term.decreasingArgs fix)
  
  unfoldable (App (Con _) args) = 
    any (Fold.any functionVar) args
    where 
    functionVar (App (Var _) (_:_)) = True
    functionVar _ = False
  unfoldable _ = False
  
unsafeUnfoldFixInj _ =
  Fail.here
-}

-- | If a fixed-point has a finite decreasing argument then we unfold 
-- the fixed-point.
unfoldFinite :: Transform.Step m => Term -> m Term
unfoldFinite (App fix@(Fix {}) args) 
  | any Term.isFinite dec_args = do
    Transform.continue (app (Term.unfoldFix fix) args)
  where
  dec_args = map (args !!) (Term.decreasingArgs fix)
unfoldFinite _ = Fail.here


