module Elea.Simplifier 
(
  run, step,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term ( Term (..), Alt (..) )
import Elea.Show
import qualified Elea.Term as Term
import qualified Elea.Foldable as Fix

run :: Term -> Term
run = Fix.rewrite step
  
step :: Term -> Maybe Term
-- Beta reduction
step (App (Lam _ rhs) arg) = Just (subst arg rhs)
  
-- Eta reduction
step (Lam _ (App f (Var 0))) = Just f

-- Case-Inj reduction
step (Case lhs _ alts)
  | inj_term:args <- Term.flattenApp lhs
  , Inj (fromEnum -> n) _ <- inj_term
  , assert (length alts > n) True
  , Alt bs alt_term <- alts !! n = Just
    . assert (length args == length bs)
    $ foldl (flip subst) alt_term args

-- Simple fix unfolding
step t@(App fix@(Fix _ rhs) arg) 
  | Term.isInj . Term.leftmost $ arg = 
    Just (App (subst fix rhs) arg)
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

step _ = Nothing

