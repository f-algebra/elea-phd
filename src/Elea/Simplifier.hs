-- | The most basic and well established simplification steps.
-- Beta-reduction, eta-reduction and case-inj-reduction
-- (reduction of pattern matches over a constructor term).
module Elea.Simplifier 
(
  run, steps, stepsM, strictVars,
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
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

run :: Term -> Term
run = Fold.rewriteSteps steps

steps :: [Term -> Maybe Term]
steps = 
  [ betaReduce
  , etaReduce
  , caseInjReduce
  ]
  
stepsM :: Monad m => [Term -> m (Maybe Term)]
stepsM = map (return .) steps
  
strictVars :: Term -> Set Index
strictVars (flattenApp -> Fix _ _ fix_t : args) = id
  . Set.intersection (Indices.free args)
  . Env.trackIndices 0
  . Fold.foldM matchedUpon
  . run
  $ unflattenApp (fix_t : args)
  where
  matchedUpon :: Term -> Env.TrackIndices Index (Set Index)
  matchedUpon (Case (Var x) _ _) = do
    offset <- ask
    if x >= offset
    then return (Set.singleton (x - offset))
    else return mempty
  matchedUpon _ = return mempty
  
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
  | inj_term : args <- flattenApp lhs
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

