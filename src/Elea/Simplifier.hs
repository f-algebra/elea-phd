module Elea.Simplifier 
(
  run
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( InnerTerm (..), Term (..), Alt (..), Note (..) )

import qualified Elea.Term as Term

run :: Note a => Term a -> Term a
run = rewrite step
  
step :: forall a . Note a => Term a -> Maybe (Term a)
step term = 
  updateNote <$> stepInner (inner term)
  where
  updateNote new_term = 
    Term (noteSimplified term new_term) (inner new_term)
  
  absurd = Just (Term (note term) Absurd)
    
  stepInner :: InnerTerm a -> Maybe (Term a)
  -- Beta reduction
  stepInner (App (inner -> Lam rhs) arg) = 
    Just $ Term.substTop arg rhs
  -- Eta reduction
  stepInner (Lam (inner -> App t (inner -> Var x)))
    | x == toEnum 0 = Just t
  -- Case-Inj reduction
  stepInner (Case lhs alts)
    | (inj_term : args) <- Term.flattenApp lhs 
    , Inj inj_n <- inner inj_term 
    , Alt alt_n alt_t <- alts !! inj_n = Just
      $ assert (length args == alt_n)
      $ assert (length alts < inj_n)
      $ foldl Term.substTop alt_t args
  -- Fixpoint unfolding
  stepInner (App fix_term@(inner -> Fix fix_body) arg)
    | Inj _ <- inner (Term.leftmost arg) = Just 
      $ Term (note term)
      $ App (Term.substTop fix_term fix_body) arg 
  -- App-Absurd
  stepInner (App (inner -> Absurd) _) = absurd
  -- Case-Absurd
  stepInner (Case (inner -> Absurd) _) = absurd
  -- Absurd-Case
  stepInner (Case _ alts)
    | all ((== Absurd) . inner . altTerm) alts = absurd
  -- No simplification
  stepInner other = 
    Nothing
  

