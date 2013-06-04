-- | This module performs term transformations which involve floating
-- lambdas or cases upwards in a term. 
-- The net effect of all these steps will
-- be lambdas then cases as high up as possible.
module Elea.Floating
(
  run, steps,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

run :: Env.Readable m => Term -> m Term
run = Fold.rewriteStepsM (Simp.stepsM ++ steps)

steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = 
  map (return .) nonMonadic ++ [unfoldFix, absurdity, varEqApply]
  where
  nonMonadic = 
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
    ]
    
varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing

{-
Need to preserve types here. Absurd is "Absurd `App` Type ty".

-- Absurd function
step (App Absurd _) = Just Absurd

-- Absurd matching
step (Case Absurd _ _) = Just Absurd
-}

absurdity :: Env.Readable m => Term -> m (Maybe Term)
absurdity term
  | isAbsurd term = do
    ty <- Err.noneM (Typing.typeOf term)
    return (Just (App Absurd ty))
  where
  isAbsurd (App (App Absurd _) _) = True
  isAbsurd (Case (App Absurd _) _ _) = True
  isAbsurd _ = False
absurdity _ = return Nothing

-- | Unfolds a 'Fix' if any arguments are a constructor term
-- which does not match a recursive call to the function itself.
-- This code desperately needs to be improved, this was just a quick solution.
-- I can think of loads of ways to make this 
-- loop with otherwise terminating code.
unfoldFix :: Env.Readable m => Term -> m (Maybe Term)
unfoldFix (flattenApp -> fix@(Fix _ _ rhs) : args@(last -> arg))
  | not (null args)
  , isInj (leftmost arg) = do
    is_pat <- isPattern arg
    return $ do
      guard (not (is_pat && matchesRecCall rhs))
      Just (unflattenApp (subst fix rhs : args))
  where
  isPattern :: Env.Readable m => Term -> m Bool
  isPattern t = do
    ms <- Env.matches
    return (t `elem` Map.elems ms)
  
  matchesRecCall :: Term -> Bool
  matchesRecCall = Env.trackIndices 0 . Fold.anyM matchingCall
    where
    matchingCall :: Term -> Env.TrackIndices Index Bool
    matchingCall (flattenApp -> Var f_var : f_args@(last -> f_arg)) 
      | length f_args == length args
      , isInj (leftmost f_arg) = do
        fix_var <- ask
        return 
          $ f_var == fix_var
          && isJust (Unifier.find arg f_arg)
    matchingCall _ = return False
unfoldFix _ =
  return Nothing

  
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
    . Fix fix_info new_fix_b
    . replaceAt 0 (stripLam strip_bs (Var (toEnum $ length strip_bs)))
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
    pat = altPattern ind_ty (toEnum n)
    alt_t = id
      . Env.replaceTerm (Indices.liftMany (length binds) cse_t) pat
      . Indices.liftMany (length binds)
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
    if any (< idx_offset) (Indices.free cse_of) 
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
constantCase (Case _ _ alts) = do
  lowered <- mapM loweredAltTerm alts
  let filtered = lowered -- filter (not . isAbsurd) lowered
  case filtered of
    [] -> return (head lowered)
    alt_t:alt_ts -> do
      guard (all (== alt_t) alt_ts)
      guard (not (containsFreeFunction alt_t))
      return alt_t
  where
  containsFreeFunction :: Term -> Bool
  containsFreeFunction term = 
    Env.trackIndices (Indices.free term) (Fold.anyM freeFunc term)
    where
    freeFunc :: Term -> Env.TrackIndices (Set Index) Bool
    freeFunc (App (Var f) _) = asks (Set.member f)
    freeFunc _ = return False

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

