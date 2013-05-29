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
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
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

-- | Unfolds a 'Fix' if the first argument is a constructor term
-- which does not match a recursive call to the function itself.
-- This code desperately needs to be improved, this was just a quick solution.
-- I can think of loads of ways to make this 
-- loop with otherwise terminating code.
unfoldFix :: Term -> Maybe Term
unfoldFix (App fix@(Fix _ rhs) arg)
  | isInj (leftmost arg) 
  , not (anyMatch rhs) = 
    return (App (subst fix rhs) arg) 
  where
  anyMatch :: Term -> Bool
  anyMatch = Env.trackIndices 0 . Fold.anyM matchingCall
    where
    matchingCall :: Term -> Env.TrackIndices Index Bool
    matchingCall (flattenApp -> [Var f_var, f_arg]) 
      | isInj (leftmost f_arg) = do
        fix_var <- ask
        return 
          $ f_var == fix_var
          && isJust (Unifier.find f_arg arg)
    matchingCall _ = return False
unfoldFix _ = mzero

{-
Need to preserve types here. Absurd is "Absurd `App` Type ty".

-- Absurd function
step (App Absurd _) = Just Absurd

-- Absurd matching
step (Case Absurd _ _) = Just Absurd

-}
