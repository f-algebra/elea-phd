module Elea.Reduce 
(
  beta
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..) )

import qualified Elea.Term as Term

beta :: Term -> Term
beta = transform step
  where
  -- The three rules of our beta reduction
  step :: Term -> Term
  
  -- Lambda reduction
  step (App (Lam _ rhs) arg) =
    beta $ Term.substTop arg rhs

  -- Case-reduction
  step (Case term alts)
    | Inj n ty <- inj = 
        Term.unflattenApp $ (alts !! n) : args
    where
    (inj:args) = Term.flattenApp term
    
  -- Fix-inj reduction
  step (App fix@(Fix _ rhs) arg) 
    | Term.isInj $ Term.function arg =
        beta $ App (Term.substTop fix rhs) arg
  
  step other = other
  

