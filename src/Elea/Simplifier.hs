module Elea.Simplifier 
(
  run, step,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( InnerTerm (..), Term (..), Alt (..),
  inner, notes, altTerm )
  
import qualified Elea.Notes.Show as Show
import qualified Elea.Term as Term

run :: Show.HasNote a => Term a -> Term a
run = rewrite step
  
step :: forall a . Show.HasNote a => Term a -> Maybe (Term a)
step term = 
  updateNote <$> stepInner (get inner term)
  where
  updateNote new_term 
  --  | trace ("\n" ++ show term ++ "\n==>\n" ++ show new_term) False = undefined
    | otherwise = Term.updateNotes term new_term
  
  absurd = Just (set inner Absurd term) 
  
  stepInner :: InnerTerm a -> Maybe (Term a)
  -- Beta reduction
  stepInner (App (get inner -> Lam rhs) arg) = 
    Just $ Term.substTop arg rhs
  -- Eta reduction
  stepInner (Lam (get inner -> App t (get inner -> Var x)))
    | x == toEnum 0 = Just t
  -- Case-Inj reduction
  stepInner (Case lhs alts)
    | (inj_term : args) <- Term.flattenApp lhs 
    , Inj inj_n <- get inner inj_term 
    , assert (length alts > inj_n) True
    , Alt alt_n alt_t <- alts !! inj_n = Just
      $ assert (length args == alt_n)
      $ assert (length alts < inj_n)
      $ foldl (flip Term.substTop) alt_t args
  -- Fixpoint unfolding
  stepInner (App fix_term@(_inner -> Fix fix_body) arg)
    | Inj _ <- _inner (Term.leftmost arg) = Just 
      $ flip (set Term.inner) term
      $ App (Term.substTop fix_term fix_body) arg
  -- App-Absurd
  stepInner (App (get inner -> Absurd) _) = absurd
  -- Case-Absurd
  stepInner (Case (get inner -> Absurd) _) = absurd
  -- Absurd-Case
  stepInner (Case _ alts)
    | all ((== Absurd) . get (inner . altTerm)) alts = absurd
  -- No simplification
  stepInner other = 
    Nothing
  

