module Elea.Reduce 
(
  beta
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )

import qualified Elea.Term as Term

beta :: Term -> Term
beta = transform step
  where
  -- The four rules of our beta reduction
  step :: Term -> Term
  
  -- Lambda reduction
  step (Term.App (Term.Lam _ _ rhs) arg) =
    beta $ Term.substOutermost arg rhs
    
  -- Type lambda reduction
  step (Term.TyApp (Term.TyLam _ rhs) ty) = 
    Term.substOutermostType ty rhs

  -- In-out reduction
  step (Term.App (Term.Out ty) (Term.App (Term.In ty') rhs)) =
    assert (ty == ty') rhs
    
  -- Fix-in reduction
  step (Term.App fix@(Term.Fix _ _ rhs) arg@(Term.App (Term.In _) _)) =
    beta $ Term.App (Term.substOutermost fix rhs) arg
  
  step other = other
  

