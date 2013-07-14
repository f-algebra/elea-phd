-- | This module performs term transformations which do 
-- not involve fixpoint fusion. The "lightweight" simplifications.
module Elea.Floating
(
  run, steps, 
  commuteMatchesWhen
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
run = Term.restrictedRewriteStepsM (Simp.stepsM ++ steps)

steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = id
  . map Typing.checkStep
  $ nonMonadic ++ [ unfoldFixInj, absurdity, varEqApply ]
  where
  nonMonadic = map (return .)
    [ constArg   
    , lambdaCase
    , funCase
    , argCase
    , freeCaseFix
    , identityCase
    , caseCase
    , raiseVarCase
    , constantCase
    , uselessFix
    , unfoldCaseFix
    , unfoldWithinFix 
    ]
    
-- | Given a predicate P, if it finds a pattern match outside (outer)
-- of another pattern match (inner), where the P(outer, inner) holds,
-- then inner is floated outside outer.
commuteMatchesWhen :: Env.Readable m => 
  (Term -> Term -> m Bool) -> Term -> m (Maybe Term)
commuteMatchesWhen when outer_cse@(Case _ _ alts) 
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
commuteMatchesWhen _ _ = return Nothing
    
varEqApply :: Env.Readable m => Term -> m (Maybe Term)
-- varEqApply term | isFix (leftmost term) || isVar term = Env.matchedWith term
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
  {-
  isAbsurd (Fix (FixInfo inf _) _ _) = 
    any absurdMatch inf
    where
    absurdMatch (leftmost -> Inj inj_n _, inj_n') = inj_n /= inj_n'
    absurdMatch _ = False
  -}
  isAbsurd _ = False
    
absurdity _ =
  return Nothing

-- | Unfolds a 'Fix' if any arguments are a constructor term
-- which does not match a recursive call to the function itself.
-- This code desperately needs to be improved, this was just a quick solution.
-- I can think of loads of ways to make this 
-- loop with otherwise terminating code.
unfoldFixInj :: Env.Readable m => Term -> m (Maybe Term)
unfoldFixInj term@(flattenApp -> fix@(Fix _ _ rhs) : args@(last -> arg))
  | not (null args)
  , isInj (leftmost arg) = do
    is_pat <- isPattern arg
    return $ do
    --  guard (not ({- is_pat -} True && matchesRecCall rhs))
      guard (is_base || not is_pat || not_looping)
      {- trace ("UNFINJFIX: " ++ show term) $ -}
      return (unflattenApp (subst fix rhs : args))
  where
  Inj inj_n ind_ty = leftmost arg
  is_base = Term.isBaseCase ind_ty inj_n
  not_looping = Term.minimumInjDepth arg > maximumRecDepth rhs
    
  maximumRecDepth :: Term -> Int
  maximumRecDepth = id
    . fromEnum
    . getMaximum 
    . Env.trackIndices 0 
    . Fold.foldM recDepth
    where
    recDepth :: Term -> Env.TrackIndices Index (Maximum Nat)
    recDepth (flattenApp -> Var f_var : f_args@(last -> f_arg)) 
      | length f_args == length args
      , isInj (leftmost f_arg) = do
        fix_var <- ask
        if f_var == fix_var
        then return (Term.injDepth f_arg)
        else return mempty
    recDepth _ = return mempty
  
  isPattern :: Env.Readable m => Term -> m Bool
  isPattern t = do
    ms <- Env.matches
    Map.elems ms
      |> map fst
      |> elem t
      |> return 

unfoldFixInj _ =
  return Nothing
  
  
-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
-- TODO: Extend this to arbitrary depth of unrolling, currently
-- it only works for depth 1.
unfoldCaseFix :: Term -> Maybe Term
unfoldCaseFix term@(Case cse_t@(leftmost -> fix@(Fix {})) ind_ty alts)
  | Term.isProductive fix
  , and (zipWith recArgsUsed [0..] alts) =
   -- trace ("UNFCASEFIX: " ++ show term) $ 
    return (Case cse_t' ind_ty alts)
  where
  fix@(Fix _ _ rhs) : args = flattenApp cse_t
  cse_t' = unflattenApp (subst fix rhs : args)
  
  -- Whether the recursive variables of a given pattern match
  -- are used down that branch
  recArgsUsed :: Nat -> Alt -> Bool
  recArgsUsed n (Alt _ alt_t) = 
    not (any isUsed args)
    where
    isUsed = (`Set.member` Indices.free alt_t)
    args = Term.recursivePatternArgs ind_ty n
  
unfoldCaseFix _ = mzero


-- | Unfolds a 'Fix' within itself if it can be unrolled at
-- at a point it is called recursively.
unfoldWithinFix :: Term -> Maybe Term
unfoldWithinFix fix@(Fix fix_i fix_b fix_t) = 
  Env.trackIndices (0, fix_t) $ do
    can_unfold <- Fold.anyM unfoldable fix_t
    if not can_unfold
    then return Nothing
    else do
      fix_t' <- Fold.transformM unfold fix_t
     -- trace ("UNFWITHFIX: " ++ show fix) $
      return (Just (Fix fix_i fix_b fix_t'))
  where
  arg_count = argumentCount fix
  
  unfoldable :: forall a . Term -> Env.TrackIndices (Index, a) Bool
  unfoldable (flattenApp -> Var f : args)
    | length args == arg_count = do
      (fix_f, _) <- ask
      return (f == fix_f && all Term.isFinite args)
  unfoldable _ =
    return False
  
  unfold :: Term -> Env.TrackIndices (Index, Term) Term
  unfold term@(flattenApp -> Var f : args) = do
    can_unfold <- unfoldable term
    if not can_unfold
    then return term
    else do
      (_, fix_t) <- ask
      return (unflattenApp (fix_t : args))
  unfold other = 
    return other
  
unfoldWithinFix _ = mzero

 
-- | Float lambdas out of the branches of a pattern match
lambdaCase :: Term -> Maybe Term
lambdaCase (Case lhs ind_ty alts)
  -- This step only works if every branch has a lambda topmost
  | all (isLam . get altInner) alts = id
    . return
    . Lam new_b
    . Case (Indices.lift lhs) (Indices.lift ind_ty) 
    $ map lctAlt alts
  where
  -- Use the binding of the first alt's lambda as our new outer binding
  getBinding (Alt bs (Lam lam_b _)) = 
    Indices.lowerMany (length bs) lam_b
  new_b = getBinding (head alts)
  
  -- Lots of careful de-Bruijn index adjustment here
  lctAlt (Alt bs (Lam lam_b rhs)) = id
    . Alt (map Indices.lift bs)
    . subst (Var (toEnum (length bs)))
    . Indices.liftAt (toEnum (length bs + 1))
    $ rhs
    
lambdaCase _ = mzero


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
funCase :: Term -> Maybe Term
funCase (App (Case lhs ind_ty alts) arg) = id
  . return
  $ Case lhs ind_ty (map appArg alts)
  where
  appArg (Alt bs rhs) =
    Alt bs (App rhs (Indices.liftMany (length bs) arg))
    
funCase _ = mzero


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
argCase :: Term -> Maybe Term
argCase (App fun (Case lhs ind_ty alts)) = id
  . return 
  $ Case lhs ind_ty (map appFun alts)
  where
  appFun (Alt bs rhs) =
    Alt bs (App (Indices.liftMany (length bs) fun) rhs)
    
argCase _ = mzero


-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
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
    $ Fold.allM isConst inner_rhs
    where
    isConst :: Term -> Env.TrackIndices (Index, Index) Bool
    isConst (flattenApp -> (Var fun_idx) : args) 
      | length args > arg_pos = do
          (fix_idx, arg_idx) <- ask
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
  -- Code like this makes me hate de-Bruijn indices, particularly since
  -- it's mostly just me not being clever enough to do it more concisely.
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
    stripLam bs = 
        unflattenLam bs 
      . applyArgs (length bs)
      where
      applyArgs n t = 
          unflattenApp 
        $ t : [ Var (toEnum i) | i <- reverse [1..n-1] ]    
      
constArg _ = mzero

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
      . Term.replace (liftHere cse_t) pat
      . liftHere
      $ inner_t


-- | If we pattern match inside a 'Fix', but only using variables that exist
-- outside of the 'Fix', then we can float this pattern match outside
-- of the 'Fix'.
freeCaseFix :: Term -> Maybe Term
freeCaseFix fix_t@(Fix _ _ fix_body) = do
  free_case <- id
    . Env.trackIndices 1
    $ Fold.findM freeCases fix_body
  return (applyCaseOf free_case fix_t)
  where
  freeCases :: Term -> Env.TrackIndices Index (Maybe Term)
  freeCases cse@(Case cse_of _ _) = do
    idx_offset <- ask
    if isVar cse_of 
      || any (< idx_offset) (Indices.free cse_of) 
    then return Nothing
    else return 
       . Just
       . Indices.lowerMany (fromEnum idx_offset) 
       $ cse
  freeCases _ = 
    return Nothing
  
freeCaseFix _ = mzero


-- | This one is mostly to get rev-rev to go through. Removes a pattern
-- match which just returns the term it is matching upon.
identityCase :: Term -> Maybe Term
identityCase (Case cse_t ind_ty alts)
  | and (zipWith isIdAlt [0..] alts) = return cse_t
  where
  isIdAlt :: Nat -> Alt -> Bool
  isIdAlt n (Alt bs alt_t) = 
    alt_t == altPattern (liftMany (length bs) ind_ty) n
identityCase _ = mzero


-- | Dunno if this ever comes up but if we have a fix without any occurrence
-- of the fix variable in the body we can just drop it.
uselessFix :: Term -> Maybe Term
uselessFix (Fix _ _ fix_t)
  | not (0 `Set.member` Indices.free fix_t) = 
      Just (Indices.lower fix_t)
uselessFix _ = mzero


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
  containsFunctionCall = Fold.any isFunctionCall
    where
    isFunctionCall (App (Fix {}) _) = True -- not (isInj fun)
    isFunctionCall _ = False

  loweredAltTerm :: Alt -> Maybe Term
  loweredAltTerm (Alt bs alt_t) = do
    guard (Indices.lowerableBy (length bs) alt_t)
    return (Indices.lowerMany (length bs) alt_t)
constantCase _ = mzero


-- | Pattern matches over variables should be above those over function
-- results.
raiseVarCase :: Term -> Maybe Term
raiseVarCase outer_t@(Case outer_of _ outer_alts)
  | isFix (leftmost outer_of) = do
    Alt bs var_case <- find varAlt outer_alts
    let var_case' = Indices.lowerMany (length bs) var_case
    return (applyCaseOf var_case' outer_t)
  where
  varAlt :: Alt -> Bool
  -- The inner case must pattern match over a variable not 
  -- bound by the outer pattern match
  varAlt (Alt bs (Case (Var idx) _ _)) = 
    fromEnum idx >= length bs
  varAlt _ = False
raiseVarCase _ = mzero


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Term -> Maybe Term
caseCase outer_cse@(Case inner_cse@(Case {}) _ _) =
  Just (applyCaseOf inner_cse outer_cse)
caseCase _ = mzero

