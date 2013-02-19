-- | This module performs term transformations which involve floating
-- lambdas or cases upwards in a term. The net effect of all these steps will
-- be lambdas then cases all at the start of a function.
module Elea.Floating
(
  steps, run,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), InnerTerm (..), Alt (..) )

import qualified Data.Label.Maybe as Maybe
import qualified Elea.Term as Term

run :: Term -> Term
run = Term.rewrite steps

steps :: Term -> Maybe Term
steps t = lambdaCaseStep t 
  `mplus` funCaseStep t
  `mplus` argCaseStep t
  `mplus` caseCaseStep t
  
-- | Float lambdas out of the branches of a pattern match
lambdaCaseStep :: Term.Notes a => Term a -> Maybe (Term a)
lambdaCaseStep old_term@(Term ns (Case lhs alts)) 
  -- This step only works if every branch has a lambda topmost
  | all (Term.isLam . get Term.altTerm) alts =   
      Just $ Term.updateNotes old_term new_term
  where
  new_term = Term (concat alt_notes) $ Lam 
    $ Term ns $ Case lifted_lhs lifted_alts 
  lifted_lhs = Term.lift lhs
  (lifted_alts, alt_notes) = unzip (map lctAlt alts) 
  
  lctAlt (Alt n (Term ns (Lam rhs))) = 
    (Alt n swapped, ns)
    where
    lifted = Term.liftAt (toEnum (n+1)) rhs
    swapped = Term.substTop (Term empty (Var (toEnum n))) lifted
    
lambdaCaseStep _ = Nothing

-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
funCaseStep :: Term.Notes a => Term a -> Maybe (Term a)
funCaseStep old_term@(get Term.inner -> App fun arg)
  | Case _ alts <- get Term.inner fun = 
      Just $ Term.updateNotes old_term new_term
  where
  new_term = 
    Maybe.modify' (Term.caseOfAlts . Term.inner) (map appArg) fun
  
  appArg (Alt n rhs) = Alt n 
    $ Term (get Term.notes old_term)
    $ App rhs (Term.liftMany n arg)
    
funCaseStep _ = Nothing

-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
argCaseStep :: Term.Notes a => Term a -> Maybe (Term a)
argCaseStep old_term@(get Term.inner -> App fun arg)
  | Case _ alts <- get Term.inner arg =
      Just $ Term.updateNotes old_term new_term
  where
  new_term =
    Maybe.modify' (Term.caseOfAlts . Term.inner) (map appFun) arg
  
  appFun (Alt n rhs) = Alt n
    $ Term (get Term.notes old_term)
    $ App (Term.liftMany n fun) rhs
    
argCaseStep _ = Nothing

-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCaseStep :: Term.Notes a => Term a -> Maybe (Term a)
caseCaseStep old_term@(Term outer_ns (Case outer_of outer_alts))
  | Case {} <- get Term.inner outer_of =
      Just $ Term.updateNotes old_term new_term
  where
  new_term = 
    Maybe.modify' (Term.caseOfAlts . Term.inner) (map floatAlt) outer_of
    
  floatAlt (Alt n rhs) = Alt n
    $ Term outer_ns
    $ Case rhs outer_alts 
    
caseCaseStep _ = Nothing
