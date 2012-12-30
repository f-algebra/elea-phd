module Elea.Reduce 
(
  simplify
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), Alt (..) )

import qualified Elea.Term as Term

simplify :: Term -> Term
simplify = rewrite step
  where
  step :: Term -> Maybe Term
  
  -- Beta reduction
  step (App (Lam rhs) arg) =
    Just $ Term.substTop arg rhs

  -- Eta reduction
  step (Lam (App t (Var x)))
    | x == toEnum 0 = Just t
    
  -- Case-Inj reduction
  step (Case (Term.flattenApp -> (Inj inj_n : args)) alts) =
    Just
    $ assert (length args == alt_n)
    $ assert (length alts < inj_n)
    $ foldl Term.substTop alt_t args
    where
    Alt alt_n alt_t = alts !! inj_n
        
  -- Fixpoint unfolding
  step (App (Fix fix) arg@(Term.leftmost -> Inj _)) = 
    Just $ App (Term.substTop (Fix fix) fix) arg
    
  -- App-Absurd
  step (App Absurd _) = 
    Just Absurd
  
  -- Case-Absurd
  step (Case Absurd _) =
    Just Absurd
    
  -- Absurd-Case
  step (Case _ alts)
    | all ((== Absurd) . altTerm) alts = Just Absurd

  step other = Nothing
  

