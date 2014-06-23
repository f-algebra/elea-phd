-- | Some QuickCheck style counterexample dynamic checking functions to 
-- help reject pointless potential simplifications.
module Elea.Checker
(
  -- constrainedToConstant
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Evaluation as Eval
import qualified Elea.Simplifier as Simp
import qualified Elea.Constraint as Constraint
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

unwrapDepth :: Nat
unwrapDepth = 1

{-
-- | Checks whether a set of constraints reduces the given recursive function
-- call to a constant.
-- No point running match-fix fusion if they don't.
constrainedToConstant :: Fail.Can m => Set Constraint -> Term -> m Term
constrainedToConstant constrs term@(App fix@(Fix {}) args) = do
  Fail.unless (length free_terms == 1)
  return (head free_terms)
  where
  result_ty = Type.getClosed term
  
  -- Uses the context composition monoid from "Elea.Context"
  constr_ctx = id
    -- Lift the indices by 1 to take into account the unwrapped fix variable
    . Indices.lift
    . concatMap (Constraint.toContext result_ty) 
    $ toList constrs
  
  -- Unwrap the fixpoint, apply the constraints and simplify the term
  simp_t = id
  --  . 
  --  . traceMe "after"
    . Simp.run
   -- . traceMe "before"
    -- Bring all pattern matches on variables topmost, so we unroll 
    -- functions based on the unwrapped fixpoint recursion scheme.
    . Eval.floatVarMatches
    -- Apply the constraints around the unwrapped fixpoint
    . Context.apply constr_ctx
    . App (Term.unwrapFix unwrapDepth fix) 
    -- Lift the indices by 1 to take into account the unwrapped fix variable
    $ map Indices.lift args

  -- Collect the possible free return terms
  free_terms = id
    . Set.toList
    . Env.trackOffset
    -- Lift by one to take into account the free fix variable
    . Env.liftTracked
    $ Fold.isoFoldM Term.branches freeTerms simp_t
    where
    -- Return any term which is free outside the original fixpoint
    freeTerms :: Term -> Env.TrackOffset (Set Term)
    freeTerms (Unr _) = 
      return mempty
    freeTerms term = do
      is_free <- Env.lowerableByOffset term
      if is_free
      then liftM Set.singleton (Env.lowerByOffset term)
      else return mempty
-}
