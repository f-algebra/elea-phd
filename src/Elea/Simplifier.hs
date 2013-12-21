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
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans

run :: Env.Readable m => Term -> m Term
run = Fold.rewriteStepsM (map Type.checkStep steps)

steps :: (Fail.Can m, Env.Readable m) => [Term -> m Term]
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
  ]
  where
  eval_steps = map (Fail.fromMaybe .) Eval.steps
  
  
-- | Remove arguments to a fixpoint if they never change in 
-- any recursive calls.
removeConstArgs :: Env.Writable m => Term -> m Term
removeConstArgs = Fold.rewriteM constArg

  
-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: (Fail.Can m, Env.Readable m) => Term -> m Term
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
absurdity :: (Fail.Can m, Env.Readable m) => Term -> m Term
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
constArg term@(Fix (Bind fix_name fix_ty) fix_t) = do
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
    . Fix fix_b'
    
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
uselessFix (Fix _ fix_t)
  | not (0 `Set.member` Indices.free fix_t) = 
    return (Indices.lower fix_t)
uselessFix _ = Fail.here


-- | Unfolds a 'Fix' if one of its arguments is a constructor term.
unfoldFixInj :: (Fail.Can m, Env.Readable m) => Term -> m Term
unfoldFixInj term@(App fix@(Fix {}) args)
  | any isConArg (Term.decreasingArgs fix) = 
    return (App (Term.unfoldFix fix) args)
  where
  -- Check whether an argument is a constructor, and does not unify
  -- with any recursive calls the function itself makes (TODO).
  isConArg :: Int -> Bool
  isConArg arg_i = isCon . leftmost $ args !! arg_i
    
unfoldFixInj _ = Fail.here

{-

-- | Given a predicate P, if it finds a pattern match outside (outer)
-- of another pattern match (inner), where the P(outer, inner) holds,
-- then inner is floated outside outer.
commuteMatchesWhenM :: Fail.Can m => 
  (Term -> Term -> m Bool) -> Term -> m (Maybe Term)
commuteMatchesWhenM when outer_cse@(Case _ _ alts) 
  | Just inner_cse <- (msum . map innerMatch) alts = do
    lets_do_it <- when outer_cse inner_cse
    if lets_do_it
    then (return . Just . applyCaseOf inner_cse) outer_cse
    else return Nothing
  where
  innerMatch :: Alt -> Maybe Term
  innerMatch (Alt bs alt_t@(Case _ _ _)) = 
    Indices.tryLowerMany (length bs) alt_t
  innerMatch _ = Nothing
commuteMatchesWhenM _ _ = return Nothing

commuteMatchesWhen :: (Term -> Term -> Bool) -> Term -> Maybe Term
commuteMatchesWhen when = 
  runIdentity . commuteMatchesWhenM ((return .) . when)

-- | Unfolds a 'Fix' if one of its arguments is a constructor term,
-- subject to a load of random, half-baked conditions.
-- This code desperately needs to be improved.
unfoldFixInj :: (Fail.Can m, Env.Readable m) => Term -> m (Maybe Term)
unfoldFixInj term@(App fix@(Fix _ _ rhs) args)
  | any (isInj . leftmost) args = do
    pat_terms <- liftM (map fst . Map.elems) Env.matches
    let is_pat = any unifiesWithPattern pat_terms
    
    from_s <- showM term
    to_s <- showM (Simp.run (App (subst fix rhs) args))
    return $ do
      guard (Term.isFinite arg || not is_pat || not_looping)
      
      let msg | Term.isFinite arg = id
              | otherwise = trace ("UNFIXINJ:\n" ++ from_s ++ "\nTO:\n" ++ to_s)
      return (app (subst fix rhs) args)
    --    |> msg  
  where
  Just arg = find (isInj . leftmost) args
  not_looping = Term.minimumInjDepth arg >= Term.recursionDepth fix
  
  unifiesWithPattern :: Term -> Bool
  unifiesWithPattern pat_term
    | Just uni <- Unifier.find pat_term arg = id
      . Set.null 
      . Set.intersection strict_pat_vars
      $ Map.keysSet uni
    where
    strict_pat_vars = id
      . Set.intersection (Indices.free pat_term)
      . Term.strictVars
      $ Term.replace arg pat_term term
  unifiesWithPattern _ = False
      
unfoldFixInj _ =
  return Nothing


-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
-- TODO: Extend this to arbitrary depth of unrolling, currently
-- it only works for depth 1.
unfoldCaseFix :: Term -> Maybe Term
unfoldCaseFix term@(Case (App fix@(Fix _ _ rhs) args) ind_ty alts)
  | Term.isProductive fix
  , Term.variablesFinitelyUsed ind_ty alts = 
    Just (Case cse_t' ind_ty alts)
  where
  cse_t' = app (subst fix rhs) args
unfoldCaseFix _ = Nothing
  

-- | Oh the grief this has caused me. It seems like an innocuous enough
-- step but jesus the infinite loops this'll give you...
-- As a reminder to myself, what it does it cause non-productive functions
-- to be expanded (like flatten), which creates a pattern-match on a variable
-- which'll float to the top and expand out a bunch of unrelated functions
-- one of which will probably contain flatten again, and so it repeats.
-- I am only using this inside a very particular step, very ad hoc I know
-- but I'll figure out a way around it at some point.
unsafeUnfoldCaseFix :: (Fail.Can m, Env.Readable m) => Term -> m (Maybe Term)
unsafeUnfoldCaseFix cse_t@(Case (leftmost -> Fix _ fix_b fix_t) _ _)
  | Term.isRecursiveInd ind_ty && Term.variablesUnused cse_t = do
    expanded <- id
      . Env.bindAt 0 fix_b
      . safeRun
      $ term'
    exp_s <- Env.bindAt 0 fix_b (showM expanded)
    from_s <- showM cse_t
    if Indices.lowerableBy 1 expanded
    then return (Just (Indices.lower expanded))
      --     |> trace ("CASEFIX:\n" ++ exp_s)
    else return Nothing
       --   |> trace ("FAILEDCASEFIX:\n" ++ from_s)
  where    
  -- We lift indices by one to allow for the recursive call of the 
  -- inner fix we will be unrolling to be at index 0.
  Case (flattenApp -> _:args) ind_ty alts = Indices.lift cse_t
  term' = Case (App fix_t args) ind_ty alts
  
  safeRun = id
    . Fold.isoRewriteStepsM Term.restricted 
    $ safeSteps ++ [unsafeUnfoldCaseFix]
unsafeUnfoldCaseFix _ = 
  return Nothing


-- | Unfolds a 'Fix' within itself if it can be unrolled at
-- at a point it is called recursively.
unfoldWithinFix :: Term -> Maybe Term
unfoldWithinFix fix@(Fix fix_i fix_b fix_t) = 
  Env.trackIndices (0, fix_t) $ do
    can_unfold <- Fold.isoAnyM Term.restricted unfoldable fix_t
    if not can_unfold
    then return Nothing
    else do
      fix_t' <- Fold.isoTransformM Term.restricted unfold fix_t
     -- trace ("UNFWITHFIX: " ++ show fix) $
      return (Just (Fix fix_i fix_b fix_t'))
  where
  unfoldable :: Term -> Env.TrackIndices (Index, Term) Bool
  unfoldable (App (Var f) args) = do
    (fix_f, _) <- Env.tracked
    return (f == fix_f && all Term.isFinite args)
  unfoldable _ =
    return False
  
  unfold :: Term -> Env.TrackIndices (Index, Term) Term
  unfold term@(App (Var f) args) = do
    can_unfold <- unfoldable term
    if not can_unfold
    then return term
    else do
      (_, fix_t) <- Env.tracked
      return (App fix_t args)
  unfold other = 
    return other
  
unfoldWithinFix _ = Nothing


-- | If a recursive function just returns the same value, regardless of its
-- inputs, just reduce it to that value.
constantFix :: Term -> Maybe Term
constantFix (Fix _ fix_b fix_t)
  | Just [result] <- mby_results = guessConstant result
  | Just [] <- mby_results = guessConstant (Absurd result_ty)
  where
  (arg_bs, _) = flattenLam fix_t
  (_, result_ty) = flattenPi (get boundType fix_b)
  mby_results = potentialResults removed_t
  
  removed_t = fix_t
    |> Fold.isoTransformM Term.restricted removeMatchedRecs
    |> Env.trackIndices 0
    where
    removeMatchedRecs :: Term -> Env.TrackIndices Index Term
    removeMatchedRecs cse@(Case cse_t _ _) = do
      fix_f <- Env.tracked
      if fix_f `Set.member` Indices.free cse_t
      then return (Absurd Set)
      else return cse
    removeMatchedRecs other = 
      return other
  
  potentialResults :: Term -> Maybe [Term]
  potentialResults = id
    . map toList
    . Env.trackIndices 0
    . runMaybeT
    . Fold.isoFoldM Term.branchesOnly resultTerm
    where
    resultTerm :: Term -> MaybeT (Env.TrackIndices Index) (Set Term)
    resultTerm (Absurd _) = return mempty
    resultTerm term = do
      fix_f <- Env.tracked
      if leftmost term == Var fix_f
      then return mempty
      else do
        let depth = fromEnum fix_f
        -- Checking we can lower by one extra makes sure there is no
        -- occurrence of the 'fix'ed variable itself.
        guard (Indices.lowerableBy (depth + 1) term)
        term
          |> Indices.lowerMany depth
          |> Set.singleton
          |> return
        
  guessConstant :: Term -> Maybe Term
  guessConstant guess_t 
    | Just [guess_t'] <- mby_results' = 
      assert (guess_t == guess_t') (Just const_t)
    | Just [] <- mby_results' = Just const_t
    | otherwise = Nothing
    where
    rec_f = guess_t
      |> Indices.liftMany (length arg_bs)
      |> unflattenLam arg_bs
      
    fix_t' = Simp.run (Term.replace (Var 0) rec_f fix_t)
    mby_results' = potentialResults fix_t'
    const_t = Indices.lower rec_f
        
constantFix _ = 
  Nothing

      
-- | If we pattern match inside a 'Fix', but only using variables that exist
-- outside of the 'Fix', then we can float this pattern match outside
-- of the 'Fix'.
freeCaseFix :: Term -> Maybe Term
freeCaseFix fix_t@(Fix _ _ fix_body) = do
  free_case <- id
    . Env.trackIndices 1
    $ Fold.isoFindM Term.restricted freeCases fix_body
  return (applyCaseOf free_case fix_t)
  where
  freeCases :: Term -> Env.TrackIndices Index (Maybe Term)
  freeCases cse@(Case cse_of _ _) = do
    idx_offset <- Env.tracked
    if any (< idx_offset) (Indices.free cse_of) 
    then return Nothing
    else return 
       . Just
       . Indices.lowerMany (fromEnum idx_offset) 
       $ cse
  freeCases _ = 
    return Nothing
  
freeCaseFix _ = Nothing


-- | This one is important to stop infinite loops. It pushes pattern matches
-- over recursive calls as far inside the term as possible (past any
-- fixpoints). Otherwise, if you unroll the function you could perform
-- fix-fact fusion using the recursive call, which would unroll the function
-- again, and so on.
caseOfRec :: forall m . (Fail.Can m, Env.Readable m) => Term -> m (Maybe Term)
caseOfRec (Fix fix_i fix_b fix_t) = id
  . liftM (fmap (Fix fix_i fix_b))
  . Env.alsoTrack 0
  . Fold.isoRewriteM' Term.restricted floatOut
  $ fix_t
  where
  floatOut :: Term -> Env.AlsoTrack Index m (Maybe Term)
  floatOut outer_cse@(Case outer_t _ _) = do
    fix_f <- Env.tracked
    let mby_inner = findInner (fromEnum fix_f)
    if leftmost outer_t /= Var fix_f 
      || isNothing mby_inner
    then return Nothing
    else do
      let Just inner_t = mby_inner 
      inner_ty <- Type.get inner_t
      return 
        . Just
        . Term.buildCaseOf inner_t inner_ty 
        $ const outer_cse
    where
    findInner :: Int -> Maybe Term
    findInner offset = id
      . Env.trackIndices 0
      . Fold.isoFindM Term.restricted floatable
      $ outer_cse
      where
      outer_vars = id
        . concatMap Indices.free
        . tail 
        $ flattenApp outer_t
      
      floatable :: Term -> Env.TrackIndices Index (Maybe Term)
      floatable term@(App fix@(Fix _ fix_b _) args)
        | outer_vars `intersects` Term.strictVars term = do
          offset <- Env.trackeds fromEnum
          return (Indices.tryLowerMany offset term)
        where
        is_rec_type = id
          . Term.isRecursiveInd
          . returnType
          $ get boundType fix_b
          
      floatable _ = 
        return Nothing
      
  floatOut _ =
    return Nothing  
      
caseOfRec _ = 
  return Nothing

  
freeFix :: (Fail.Can m, Env.Readable m) => Term -> m (Maybe Term)
freeFix outer_fix@(Fix _ _ outer_body)
  | Just free_fix <- mby_free_fix = do
    ind_ty <- Type.get free_fix
    -- While this step will work for recursive inductive types,
    -- it doesn't make much sense to me to ever do this.
    if Term.isRecursiveInd ind_ty
    then return Nothing
    else return 
       . Just
       . Term.buildCaseOf free_fix ind_ty 
       $ const outer_fix
  where
  mby_free_fix = id
    . Env.trackIndices 1
    $ Fold.isoFindM Term.restricted freeFixes outer_body
  
  freeFixes :: Term -> Env.TrackIndices Index (Maybe Term)
  freeFixes term@(App fix@(Fix {}) args) = do
      idx_offset <- Env.tracked
      return 
        . Indices.tryLowerMany (fromEnum idx_offset)
        $ term
  freeFixes _ = 
    return Nothing
  
freeFix _ = return Nothing


-- | Removes a pattern match if every branch returns the same value.
constantCase :: Term -> Maybe Term
constantCase cse_t@(Case _ _ alts) = do
  lowered <- mapM loweredAltTerm alts
  let filtered = lowered -- filter (not . isAbsurd) lowered
  case filtered of
    [] -> return (head lowered)
    alt_t:alt_ts -> do
      guard (all (== alt_t) alt_ts)
   --   guard (not (containsFunctionCall alt_t))
      return alt_t
  where
  containsFunctionCall :: Term -> Bool
  containsFunctionCall = Fold.isoAny Term.restricted isFunctionCall
    where
    isFunctionCall (App (Fix {}) _) = True -- not (isInj fun)
    isFunctionCall _ = False

  loweredAltTerm :: Alt -> Maybe Term
  loweredAltTerm (Alt bs alt_t) = do
    guard (Indices.lowerableBy (length bs) alt_t)
    return (Indices.lowerMany (length bs) alt_t)
constantCase _ = Nothing


-- | Pattern matches over variables should be above those over function
-- results.
raiseVarCase :: Term -> Maybe Term
raiseVarCase = commuteMatchesWhen raiseVar
  where
  raiseVar (Case cse_of _ _) (Case (Var _) _ _) =
    isFix (leftmost cse_of)
  raiseVar _ _ = False
  

-}
