-- | This module performs term transformations which involve floating
-- lambdas or cases upwards in a term. 
-- The net effect of all these steps will
-- be lambdas then cases all at the start of a function.
module Elea.Floating
(
  run, steps,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import Elea.Term ( Term (..), Alt (..) )
import Elea.Type ( Type )
import qualified Elea.Type as Type
import qualified Elea.Term as Term
import qualified Elea.Simplifier as Simp
import qualified Elea.Foldable as Fold
import qualified Data.Monoid as Monoid

run :: Term -> Term
run = Fold.rewriteSteps (Simp.steps ++ steps)

steps :: [Term -> Maybe Term]
steps = 
  [ lambdaCaseStep 
  --, funCaseStep
  , argCaseStep
  , constArgStep
  ]
  
-- | Float lambdas out of the branches of a pattern match
lambdaCaseStep :: Term -> Maybe Term
lambdaCaseStep (Case lhs ind_ty alts)
  -- This step only works if every branch has a lambda topmost
  | all (Term.isLam . get Term.altInner) alts = 
        return
      . Lam new_b
      . Case (lift lhs) (lift ind_ty) 
      $ map lctAlt alts
  where
  -- Use the binding of the first alt's lambda as our new outer binding
  getBinding (Alt bs (Lam lam_b _)) = 
    modify Type.boundType (lowerMany (length bs)) lam_b
  new_b = getBinding (head alts)
  
  -- Lots of careful de-Bruijn index adjustment here
  lctAlt (Alt bs (Lam lam_b rhs)) = 
      Alt (map Type.liftBind bs)
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
  return . Simp.run . removeConstArg $ pos
  where
  (arg_binds, inner_rhs) = Term.flattenLam fix_rhs
  fix_index = toEnum (length arg_binds)
  
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
  -- Code like this makes me hate de-Bruijn indices, particularly since
  -- it's mostly just me not being clever enough to do it cleanly.
  removeConstArg :: Int -> Term
  removeConstArg arg_pos =
      stripLam (map Type.lowerBind strip_bs)
    . liftManyAt (length strip_bs - 1) 1
    . Fix new_fix_b
    . replaceAt 0 (stripLam strip_bs (Term.Var new_index))
    . Term.unflattenLam (map Type.liftBind left_bs)
    . subst (Term.Var new_index)
    . Term.unflattenLam (map Type.liftBind right_bs)
    . liftAt (toEnum $ length arg_binds + 1)
    $ inner_rhs
    where
    -- Split the lambda bindings up 
    -- at the position where we are removing one.
    (left_bs, dropped_b:right_bs) = splitAt arg_pos arg_binds
    strip_bs = left_bs ++ [dropped_b]
    
    new_index = toEnum (length left_bs + 1)
    
    -- Update the type of the bound fix variable to have one less argument
    new_fix_b = modify Type.boundType (lift . Type.removeArg arg_pos) fix_b
    
    -- Abstracts new_bs, and reapplies all but the last binding,
    -- like eta-equality which skipped the last binding.
    -- E.g. @stripLam [A,B,C] f == fun (_:A) (_:B) (_:C) -> f _2 _1@
    stripLam :: [Type.Bind] -> Term -> Term
    stripLam bs = 
        Term.unflattenLam bs 
      . applyArgs (length bs)
      where
      applyArgs n t = 
          Term.unflattenApp 
        $ t : [ Term.Var (toEnum i) | i <- reverse [1..n-1] ]    
      
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

