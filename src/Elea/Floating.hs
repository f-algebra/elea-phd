-- | This module performs term transformations which involve floating
-- lambdas or cases upwards in a term. 
-- The net effect of all these steps will
-- be lambdas then cases all at the start of a function.
module Elea.Floating
(
  steps, run,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import Elea.Term ( Term (..), Alt (..) )
import qualified Elea.Term as Term
import qualified Elea.Foldable as Fold
import qualified Data.Monoid as Monoid

run :: Term -> Term
run = Fold.rewrite steps

steps :: Term -> Maybe Term
steps t = lambdaCaseStep t 
  `mplus` funCaseStep t
  `mplus` argCaseStep t
  `mplus` constArgStep t
--  `mplus` caseCaseStep t
  
-- | Float lambdas out of the branches of a pattern match
lambdaCaseStep :: Term -> Maybe Term
lambdaCaseStep (Case lhs ind_ty alts)
  -- This step only works if every branch has a lambda topmost
  | all (Term.isLam . get Term.altInner) alts = 
        return
      . Lam new_b
      $ Case (lift lhs) ind_ty (map lctAlt alts) 
  where
  -- Use the binding of the first alt's lambda as our new outer binding
  getBinding (Alt _ (Lam lam_b _)) = lam_b
  new_b = getBinding (head alts)
  
  -- Lots of careful de-Bruijn index adjustment here
  lctAlt (Alt bs (Lam lam_b rhs)) = 
      Alt bs 
    . subst (Var (toEnum (length bs)))
    . liftAt (toEnum (length bs + 1))
    $ rhs
    
lambdaCaseStep _ = mzero


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
funCaseStep :: Term -> Maybe Term
funCaseStep (App (Case lhs ind_ty alts) arg) =
    return
  $ Case lhs ind_ty (map appArg alts)
  where
  appArg (Alt bs rhs) =
    Alt bs (App rhs (liftMany (length bs) arg))
    
funCaseStep _ = mzero


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
argCaseStep :: Term -> Maybe Term
argCaseStep (App fun (Case lhs ind_ty alts)) =
    return 
  $ Case lhs ind_ty (map appFun alts)
  where
  appFun (Alt bs rhs) =
    Alt bs (App (liftMany (length bs) fun) rhs)
    
argCaseStep _ = mzero


-- | If an argument to a 'Fix' never changes in any recursive call
-- then we should float that lambda abstraction outside the 'Fix'.
constArgStep :: Term -> Maybe Term
constArgStep (Fix fix_b fix_rhs) = do
  pos <- find isConstArg [0..length arg_binds - 1]
  return (stripArg pos)
  where
  (arg_binds, inner_rhs) = Term.flattenLam fix_rhs
  fix_index = 0 + toEnum (length arg_binds)
  
  argIndex :: Int -> Index
  argIndex arg_pos = 
    toEnum (length arg_binds - (arg_pos + 1))
  
  isConstArg :: Int -> Bool
  isConstArg arg_pos = 
      flip runReader (fix_index, argIndex arg_pos) 
    . Term.ignoreFacts
    $ Fold.allM isConst inner_rhs
    where
    -- Because we have the instance Type.Env (Reader (Index, Index)), 
    -- these indices will be correctly incremented as we descend
    -- into the term.
    isConst :: Term -> Term.IgnoreFactsT (Reader (Index, Index)) Bool
    isConst (Term.flattenApp -> (Term.Var fun_idx) : args) 
      | length args > arg_pos = do
          (fix_idx, arg_idx) <- ask
          let arg = args !! arg_pos
              Term.Var var_idx = arg
          return 
            $ fix_idx /= fun_idx
            || not (Term.isVar arg)
            || var_idx == arg_idx
    isConst _ = 
      return True

  -- Returns the original argument to constArgStep, with the given
  -- argument floated outside of the 'Fix'.
  stripArg :: Int -> Term
  stripArg arg_pos =
      Term.unflattenLam outer_binds
    . applyArgs
    . Fix fix_b
    . Term.unflattenLam (removeAt arg_pos arg_binds)
    . substAt old_index (Term.Var new_index) 
    $ inner_rhs
    where
    old_index = argIndex arg_pos
    new_index = toEnum (length arg_binds + 1)
    outer_binds = take arg_pos arg_binds
    
    applyArgs :: Term -> Term
    applyArgs f = 
        Term.unflattenApp 
      $ f : [ Term.Var (toEnum i) | i <- reverse [1..arg_pos] ]
      
constArgStep _ = mzero


{- This needs careful index adjustment for the new inner alts I think
-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCaseStep :: Term -> Maybe Term
caseCaseStep (Case (Case lhs inner_ty inner_alts) outer_ty outer_alts) =
  Case lhs inner_ty (map floatAlt inner_alts)
  where
  floatAlt (Alt bs rhs) = 
    Alt bs (Case rhs outer_ty outer_alts)
    
caseCaseStep _ = Nothing
-}
