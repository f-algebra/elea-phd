-- | The most basic and well established simplification steps.
-- Beta-reduction, eta-reduction and case-inj-reduction
-- (reduction of pattern matches over a constructor term).
module Elea.Evaluation 
(
  run, steps
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
  , beta
  , eta
  , caseOfCon
  ]

normaliseApp :: Term -> Maybe Term
normaliseApp (App f []) = return f
normaliseApp (App (App f ts1) ts2) = return (App f (ts1 ++ ts2))
normaliseApp _ = mzero
  
beta :: Term -> Maybe Term
beta (App (Lam _ rhs) (arg:args)) = 
  return (app (subst arg rhs) args)
beta _ = mzero
  
eta :: Term -> Maybe Term
eta (Lam _ (App f xs@(last -> Var 0)))
  | not (0 `Set.member` Indices.free new_t) = 
    return (Indices.lower new_t)
  where
  new_t = app f (init xs)
eta _ = mzero

caseOfCon :: Term -> Maybe Term
caseOfCon (Case ind cse_t alts)
  | Con _ (fromEnum -> n) : args <- flattenApp cse_t
  , Alt bs alt_t <- alts !! n = id
    . return
    -- We fold substitute over the arguments to the constructor
    -- starting with the return value of the pattern match (alt_t).
    -- So we substitute each argument in one by one to the alt term.
    . foldr subst alt_t
    -- When we substitute an constructor argument, 
    -- it needs to not be affected by the substitution of later arguments.
    -- So we lift their indices a number of times
    -- depending on their position in the order of substitution, 
    -- viz. those substituted first are lifted the most.
    $ zipWith liftMany [0..] args
caseOfCon _ = mzero

