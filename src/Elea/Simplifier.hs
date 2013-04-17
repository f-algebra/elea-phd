module Elea.Simplifier 
(
  run, steps, safe,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Term as Term
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

run :: Term -> Term
run = Fold.rewriteSteps steps

safe :: Term -> Term
safe = Fold.rewriteSteps safeSteps 

steps :: [Term -> Maybe Term]
steps = safeSteps ++ [ unfoldFix ]

safeSteps :: [Term -> Maybe Term]
safeSteps = 
  [ betaReduce
  , etaReduce
  , caseInjReduce
  ]
  
betaReduce :: Term -> Maybe Term
betaReduce (App (Lam _ rhs) arg) = 
  return (subst arg rhs)
betaReduce _ = mzero
  
etaReduce :: Term -> Maybe Term
etaReduce (Lam _ (App f (Var 0)))
  | not (0 `Set.member` Indices.free f) = 
    return (Indices.lower f)
etaReduce _ = mzero

caseInjReduce :: Term -> Maybe Term
caseInjReduce (Case lhs _ alts)
  | inj_term:args <- flattenApp lhs
  , Inj (fromEnum -> n) _ <- inj_term
  , assert (length alts > n) True
  , Alt bs alt_term <- debugNth "yoyo" alts n =
      return
    . assert (length args == length bs)
    . foldr subst alt_term
    -- When we substitute an constructor argument, 
    -- it needs to not be affected by the substitution of later arguments.
    -- So we lift their indices a number of times
    -- depending on their position in the order of substitution, 
    -- viz. those substituted first are lifted the most.
    $ zipWith liftMany [0..] args
caseInjReduce _ = mzero

unfoldFix :: Term -> Maybe Term
unfoldFix (App fix@(Fix _ rhs) arg) 
  | isInj (leftmost arg) = 
    return (App (subst fix rhs) arg)
unfoldFix _ = mzero

{-
Need to preserve types here. Absurd is "Absurd `App` Type ty".

-- Absurd function
step (App Absurd _) = Just Absurd

-- Absurd matching
step (Case Absurd _ _) = Just Absurd

-- Absurd branched
step (Case _ _ alts)
  | all (== Absurd) (map (get Term.altTerm) alts) = Just Absurd  
-}
