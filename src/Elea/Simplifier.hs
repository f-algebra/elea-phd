-- | The most basic and well established simplification steps.
-- Beta-reduction, eta-reduction and case-inj-reduction
-- (reduction of pattern matches over a constructor term).
module Elea.Simplifier 
(
  run, steps, stepsM, 
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

run :: Term -> Term
run = Fold.rewriteSteps steps

steps :: [Term -> Maybe Term]
steps = 
  [ normaliseApp
  , betaReduce
  , etaReduce
  , caseInjReduce
  ]
  
stepsM :: Monad m => [Term -> m (Maybe Term)]
stepsM = map (return .) steps

normaliseApp :: Term -> Maybe Term
normaliseApp (App f []) = return f
normaliseApp (App (App f ts1) ts2) = return (App f (ts1 ++ ts2))
normaliseApp _ = mzero
  
betaReduce :: Term -> Maybe Term
betaReduce (App (Lam _ rhs) (arg:args)) = 
  return (app (subst arg rhs) args)
betaReduce _ = mzero
  
etaReduce :: Term -> Maybe Term
etaReduce (Lam _ (App f xs@(last -> Var 0)))
  | not (0 `Set.member` Indices.free new_t) = 
    return (Indices.lower new_t)
  where
  new_t = app f (init xs)
etaReduce _ = mzero

caseInjReduce :: Term -> Maybe Term
caseInjReduce (Case lhs _ alts)
  | inj_term : args <- flattenApp lhs
  , Inj (fromEnum -> n) _ <- inj_term
  , assert (length alts > n) True
  , Alt bs alt_term <- alts !! n = id
    . return
    . assert (length args == length bs)
    . foldr subst alt_term
    -- When we substitute an constructor argument, 
    -- it needs to not be affected by the substitution of later arguments.
    -- So we lift their indices a number of times
    -- depending on their position in the order of substitution, 
    -- viz. those substituted first are lifted the most.
    $ zipWith liftMany [0..] args
caseInjReduce _ = mzero

