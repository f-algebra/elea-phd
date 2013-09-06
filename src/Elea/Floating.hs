-- | This module performs term transformations which do 
-- not involve fixpoint fusion. The "lightweight" simplifications.
module Elea.Floating
(
  run, steps, 
  commuteMatchesWhenM,
  commuteMatchesWhen,
  removeConstArgs,
  unfoldCaseFix,
  unsafeUnfoldCaseFix,
  caseOfRec,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Unifier as Unifier
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

{-# INLINEABLE run #-}
run :: Env.Readable m => Term -> m Term
run = Fold.isoRewriteStepsM Term.restricted (Simp.stepsM ++ steps)

removeConstArgs :: Term -> Term
removeConstArgs = Simp.run . Fold.isoRewrite Term.restricted constArg

steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = id
  . map Typing.checkStep
  $ safeSteps ++ unsafe
  where
  unsafe =  
    [ const (return Nothing)
    , unfoldFixInj
    , return . unfoldCaseFix
    , return . unfoldWithinFix
    , unsafeUnfoldCaseFix
    ]

safeSteps :: Env.Readable m => [Term -> m (Maybe Term)]
safeSteps = 
  Simp.stepsM ++ (map (return .) safe) ++ safeM
  where
  safe =  
    [ const Nothing
    , constArg
    , caseApp
    , appCase
    , freeCaseFix
    , identityCase
    , caseCase
    , raiseVarCase
    , constantCase
    , uselessFix
    , constantFix
    ]
    
  safeM = 
    [ const (return Nothing)
    , caseFun
    , absurdity
    , freeFix
  --  , caseOfRec
    , varEqApply 
    ]
    
-- | Given a predicate P, if it finds a pattern match outside (outer)
-- of another pattern match (inner), where the P(outer, inner) holds,
-- then inner is floated outside outer.
commuteMatchesWhenM :: Monad m => 
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
  

varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing


absurdity :: Env.Readable m => Term -> m (Maybe Term)
absurdity (Absurd _) = return Nothing
absurdity term
  | isAbsurd term = do
    ty <- Err.noneM (Typing.typeOf term)
    return (Just (Absurd ty))
  where
  isAbsurd (App (Absurd _) _) = True
  isAbsurd (Case (Absurd _) _ _) = True
  -- For now we assume fixpoints are strict in all their arguments.
  isAbsurd (App fun args)
    | isFix fun || isInj fun
    , any Term.isAbsurd args = True
  isAbsurd _ = False
    
absurdity _ =
  return Nothing
 

-- | Unfolds a 'Fix' if one of its arguments is a constructor term,
-- subject to a load of random, half-baked conditions.
-- This code desperately needs to be improved.
unfoldFixInj :: Env.Readable m => Term -> m (Maybe Term)
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
unsafeUnfoldCaseFix :: Env.Readable m => Term -> m (Maybe Term)
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

 
-- | Float lambdas out of the branches of a pattern match
caseFun :: Env.Readable m => Term -> m (Maybe Term)
caseFun cse@(Case lhs ind_ty alts) 
  | any (\t -> isFix t || isLam t) alt_ts = do
    Pi arg_b _ <- Err.noneM (Typing.typeOf cse)
    return
      . Just
      . Lam arg_b
      . Case (Indices.lift lhs) (Indices.lift ind_ty)
      $ map appAlt alts
  where
  alt_ts = map (get altInner) alts
  
  appAlt (Alt bs alt_t) = id
    . Alt (map Indices.lift bs)
    . Simp.run
    $ App alt_t' [arg]
    where
    alt_t' = Indices.liftAt (toEnum (length bs)) alt_t
    arg = Var (toEnum (length bs))
    
caseFun _ = return Nothing


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Term -> Maybe Term
caseApp (App (Case lhs ind_ty alts) args) = id
  . return
  $ Case lhs ind_ty (map appArg alts)
  where
  appArg (Alt bs rhs) =
    Alt bs (App rhs (Indices.liftMany (length bs) args))
    
caseApp _ = Nothing


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
appCase :: Term -> Maybe Term
appCase term@(App _ args)
  | Just cse_t <- find isCase args =
    Just (applyCaseOf cse_t term)
appCase _ = Nothing


-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
-- This function is mostly horrific de-Brujin index shifting.
-- I coded it almost entirely by trial and error.
constArg :: Term -> Maybe Term
constArg (Fix fix_info fix_b fix_rhs) = do
  -- Find if any arguments never change in any recursive calls, 
  -- a "constant" argument, pos is the position of such an argument
  pos <- find isConstArg [0..length arg_binds - 1]
  
  -- Then we run the 'removeConstArg' function on that position, and
  -- simplify the result
  return . Simp.run . removeConstArg $ pos
  where
  (arg_binds, inner_rhs) = flattenLam fix_rhs
  fix_index = toEnum (length arg_binds)
  
  argIndex :: Int -> Index
  argIndex arg_pos = 
    toEnum (length arg_binds - (arg_pos + 1))
  
  isConstArg :: Int -> Bool
  isConstArg arg_pos = id
    . Env.trackIndices (fix_index, argIndex arg_pos) 
    $ Fold.isoAllM Term.restricted isConst inner_rhs
    where
    isConst :: Term -> Env.TrackIndices (Index, Index) Bool
    isConst (flattenApp -> (Var fun_idx) : args) 
      | length args > arg_pos = do
          (fix_idx, arg_idx) <- Env.tracked
          let arg = args !! arg_pos
              Var var_idx = arg
          return 
            -- If we aren't dealing the right function, then just return true
            $ fix_idx /= fun_idx
            -- Otherwise, check to make sure the argument hasn't changed 
            || (isVar arg && var_idx == arg_idx)
    isConst _ = 
      return True

  -- Returns the original argument to constArg, with the argument
  -- at the given index floated outside of the 'Fix'.
  removeConstArg :: Int -> Term
  removeConstArg arg_pos = id
    . Indices.lower
    . stripLam strip_bs
    . liftManyAt (length strip_bs) 1
    . Fix (Indices.lift fix_info) new_fix_b
    . replaceAt 0 (stripLam strip_bs' (Var (toEnum $ length strip_bs)))
    . substAt Indices.omega (Var 1)
    . Indices.liftAt 1
    . unflattenLam left_bs
    . subst (Var Indices.omega)
    . unflattenLam right_bs
    $ inner_rhs
    where
    -- Split the lambda bindings up 
    -- at the position where we are removing one.
    (left_bs, dropped_b:right_bs) = splitAt arg_pos arg_binds
    strip_bs = left_bs ++ [dropped_b]
    strip_bs' = zipWith Indices.liftAt [1..] strip_bs
    
    -- Generate the type of our new fix binding from the old one
    new_fix_b = id
      . Bind lbl
      . substAt Indices.omega (Var 0)
      . Indices.lift
      . unflattenPi start_bs
      . subst (Var Indices.omega)
      . unflattenPi end_bs
      $ result_ty
      where
      Bind lbl (flattenPi -> (arg_tys, result_ty)) = fix_b
      (start_bs, _:end_bs) = splitAt arg_pos arg_tys

    -- Abstracts new_bs, and reapplies all but the last binding,
    -- like eta-equality which skipped the last binding.
    -- E.g. @stripLam [A,B,C] f == fun (_:A) (_:B) (_:C) -> f _2 _1@
    stripLam :: [Bind] -> Term -> Term
    stripLam bs = id
      . unflattenLam bs 
      . applyArgs (length bs)
      where
      applyArgs n t = id
        . app t
        $ [ Var (toEnum i) | i <- reverse [1..n-1] ]    
      
constArg _ = Nothing


-- | This isn't a step, but it's used in three steps below.
-- It takes a case-of term and replaces the result term down each branch
-- with the provided term.
applyCaseOf :: Term -> Term -> Term
applyCaseOf (Case cse_t ind_ty old_alts) inner_t = 
  Case cse_t ind_ty alts
  where
  alts = zipWith mkAlt [0..] old_alts
  
  mkAlt :: Int -> Alt -> Alt
  mkAlt n (Alt binds _) = Alt binds alt_t
    where
    liftHere = Indices.liftMany (length binds)
    pat = altPattern (liftHere ind_ty) (toEnum n)
    alt_t = id
      . Term.replaceRestricted (liftHere cse_t) pat
      . liftHere
      $ inner_t

      
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
caseOfRec :: forall m . Env.Readable m => Term -> m (Maybe Term)
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
      inner_ty <- Err.noneM (Typing.typeOf inner_t)
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

  
freeFix :: Env.Readable m => Term -> m (Maybe Term)
freeFix outer_fix@(Fix _ _ outer_body)
  | Just free_fix <- mby_free_fix = do
    ind_ty <- Err.noneM (Typing.typeOf free_fix)
    -- While this step will work for recursive inductive types,
    -- it doesn't make much sense to me to ever do this.
    if Term.isRecursiveInd (if not (isInd ind_ty) then error (show free_fix) else ind_ty)
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


-- | This one is mostly to get rev-rev to go through. Removes a pattern
-- match which just returns the term it is matching upon.
identityCase :: Term -> Maybe Term
identityCase (Case cse_t ind_ty alts)
  | and (zipWith isIdAlt [0..] alts) = return cse_t
  where
  isIdAlt :: Nat -> Alt -> Bool
  isIdAlt n (Alt bs alt_t) = 
    alt_t == altPattern (liftMany (length bs) ind_ty) n
identityCase _ = Nothing


-- | Dunno if this ever comes up but if we have a fix without any occurrence
-- of the fix variable in the body we can just drop it.
uselessFix :: Term -> Maybe Term
uselessFix (Fix _ _ fix_t)
  | not (0 `Set.member` Indices.free fix_t) = 
    Just (Indices.lower fix_t)
uselessFix _ = Nothing


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
  

-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Term -> Maybe Term
caseCase outer_cse@(Case inner_cse@(Case {}) _ _) =
  Just (applyCaseOf inner_cse outer_cse)
caseCase _ = Nothing

