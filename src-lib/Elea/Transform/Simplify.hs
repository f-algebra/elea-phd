-- | An extension of 'Elea.Transform.Evaluate'. Slightly more heavyweight
-- transformations but fundamentally those which make terms smaller and simpler.
-- They do not require fusion and they do not need to make use of 'Elea.Embed'.
module Elea.Transform.Simplify
(
  Step,
  run, 
  quick,
  steps,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Embed as Embed
import qualified Elea.Term.Ext as Term
import qualified Elea.Term.Height as Height
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Transform.Names as Name
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Equality as Equality
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Fedd as Fedd  

import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Poset as Partial
import qualified Control.Monad.Trans as Trans

type Step m =
  ( Eval.Step m
  , Env.All m
  , Defs.Read m )

run :: Term -> Term
run = id
  . Fedd.eval
  . Transform.fix (Transform.compose all_steps)
  where
  all_steps = Eval.transformSteps 
    ++ steps 
    ++ Eval.traverseSteps
    ++ Equality.steps                                    

quick :: Term -> Term
quick = run

steps :: Step m => [Term -> m Term]
steps =
  [ const Fail.here
  , caseApp
  , appCase
  , caseCase
  , caseFun
  , constArg
  , identityCase
  , constantCase
  , constantFix
  , identityFix
  , unfold
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
    

-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: Step m => Term -> m Term
caseFun cse@(Case t alts) 
  | Just new_b <- potentialBinds alts = do
    alts' <- id
      . Env.bind new_b 
      $ mapM (appAlt . Indices.lift) alts
    Transform.continue
      . Lam new_b
      $ Case (Indices.lift t) alts'
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
  
  appAlt (Alt con bs alt_t) = do
    alt_t' <- id
      . Env.bindMany bs
      . Transform.continue
      $ app alt_t [arg]
    return (Alt con bs alt_t')
    where
    arg = Var (length bs)
    
caseFun _ = Fail.here
  
  
-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
constArg :: Step m => Term -> m Term
constArg (App fix@(Fix fix_info (Bind fix_name fix_ty) fix_t) args)
  | length arg_bs /= nlength args = Fail.here
  | otherwise = do
    -- Find if any arguments never change in any recursive calls
    pos <- Fail.fromMaybe (find isConstArg [0..length arg_bs - 1])
    
    -- Then we run the 'removeConstArg' function on that position to
    -- get a new fixed-point
    let fix' = id
        --  . trace ("\n\n[const-arg] trying position " ++ show pos) 
          $ removeConstArg pos
    
    -- Might as well simplify the constant argument before pushing it inside
    -- and possible duplicating it
    arg' <- Transform.continue (args !! pos)
    let args' = setAt pos arg' args
    
    -- Run evaluation to reduce all the new lambdas
    let term' = Eval.reduce fix' args'
    Transform.continue (Eval.reduce fix' args')
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
    . Fix fix_info fix_b'
    
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


-- | Removes a pattern match which just returns the term it is matching upon.
identityCase :: Step m => Term -> m Term
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

identityFix :: Step m => Term -> m Term
identityFix (App fix@(Fix _ _ fix_t@(Lam lam_b _)) [arg]) 
  | Type.get fix == new_ty
  , run (Indices.subst id_fun fix_t) == id_fun = 
    Transform.continue arg
  where
  arg_ty = get Type.bindType lam_b
  new_ty = Type.Fun arg_ty arg_ty
  id_fun = Lam lam_b (Var 0)
  
identityFix _ = Fail.here

-- | If a recursive function just returns the same value, regardless of its
-- inputs, just reduce it to that value.
constantFix :: Step m => Term -> m Term
constantFix t@(App (Fix _ fix_b fix_t) args)
  | length args /= nlength arg_bs = Fail.here
   
  | Just [result] <- mby_results
  , correctGuess result = 
    Transform.continue result
  
  | Just [] <- mby_results
  , correctGuess (Bot result_ty) = 
    return (Bot result_ty)
  
  where
  (arg_bs, _) = flattenLam fix_t
  fix_ty = get Type.bindType fix_b
  result_ty = Type.returnType fix_ty
  
  mby_results = id
    . potentialResults
    . Eval.run
    $ Indices.substAt 0 (Bot fix_ty) fix_t
  
  potentialResults :: Term -> Maybe [Term]
  potentialResults = id
    . map toList
    . Env.trackOffset
    . runMaybeT
    . Fold.isoFoldM Term.branches resultTerm
    where
    resultTerm :: Term -> MaybeT Env.TrackOffset (Set Term)
    resultTerm term
      | Term.isBot term = return mempty
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
constantCase :: forall m . Step m => Term -> m Term
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
caseApp :: Step m => Term -> m Term
caseApp (App (Case t alts) args) =
  Transform.continue (Case t (map appArg alts))
  where
  appArg (Alt con bs alt_t) =
    Alt con bs (app alt_t (Indices.liftMany (length bs) args))
    
caseApp _ = Fail.here


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
-- TODO check for strict args!!
appCase :: Step m => Term -> m Term
appCase term@(App f xs) = do
  cse_i <- Fail.fromMaybe (findIndex isCase xs)
  Transform.continue (applyCase cse_i)
  where
  applyCase cse_i = 
    Case cse_t (map applyAlt alts)
    where
    Case cse_t alts = xs !! cse_i
    
    applyAlt (Alt con bs alt_t) = 
      Alt con bs (app f' xs')
      where
      f' = Indices.liftMany (length bs) f
      xs' = id 
        . setAt cse_i alt_t
        $ Indices.liftMany (length bs) xs
      
appCase _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Step m => Term -> m Term
caseCase outer_cse@(Case inner_cse@(Case inner_t inner_alts) outer_alts) =
  Transform.continue (Case inner_t (map newOuterAlt inner_alts))
  where
  newOuterAlt :: Alt -> Alt
  newOuterAlt (Alt con bs t) = 
    Alt con bs (Eval.run (Case t alts_here))
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


unfold :: Step m => Term -> m Term
unfold term@(App fix@(Fix {}) args) 
  | any (isCon . leftmost) dec_args =
    History.check Name.Unfold term $ do
      term' <- Transform.continue (app (Term.unfoldFix fix) args)
      Fail.when (term Embed.<= term')
      return 
      --  . trace ("\n\n[unfold] " ++ show term ++ "\n\n[into] " ++ show term') 
        $ term'
  where
  dec_args = map (args !!) (Term.decreasingArgs fix)
  
unfold _ = Fail.here



