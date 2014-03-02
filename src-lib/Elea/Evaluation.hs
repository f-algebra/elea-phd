-- | The most basic and well established simplification steps.
module Elea.Evaluation 
(
  run, steps,
  strictVars,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Terms as Term
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set

run :: Term -> Term                           
run = Fold.rewriteSteps steps

steps :: Fail.Can m => [Term -> m Term]
steps = 
  [ normaliseApp
  , eta 
  , beta
  , caseOfCon
  , caseApp
  , appCase
  , caseCase 
  ]
  
  
-- | The variables whose value must be known in order to unroll the given term.
-- If we expanded this term these variables would be inside pattern matches
-- topmost.
strictVars :: Term -> Set Index
strictVars (App (Fix _ _ fix_t) args) = id
  . Set.intersection (Indices.free args)
  . Env.trackIndices 0
  . Fold.foldM matchedUpon
  . run
  $ App fix_t args
  where
  matchedUpon :: Term -> Env.TrackIndices Index (Set Index)
  matchedUpon (Case _ (Var x) _) = do
    offset <- Env.tracked
    if x >= offset
    then return (Set.singleton (x - offset))
    else return mempty
  matchedUpon _ = 
    return mempty


normaliseApp :: Fail.Can m => Term -> m Term
normaliseApp (App f []) = return f
normaliseApp (App (App f ts1) ts2) = return (App f (ts1 ++ ts2))
normaliseApp _ = Fail.here
  

beta :: Fail.Can m => Term -> m Term
beta (App (Lam _ rhs) (arg:args)) = 
  return (app (subst arg rhs) args)
beta _ = Fail.here
  

eta :: Fail.Can m => Term -> m Term
eta (Lam _ (App f xs@(last -> Var 0)))
  | not (0 `Set.member` Indices.free new_t) = 
    return (Indices.lower new_t)
  where
  new_t = app f (init xs)
eta _ = Fail.here


caseOfCon :: Fail.Can m => Term -> m Term
caseOfCon (Case ind cse_t alts)
  | Con _ (fromEnum -> n) : args <- flattenApp cse_t
  , Alt bs alt_t <- alts !! n = id
    . return
    -- We fold substitute over the arguments to the constructor
    -- starting with the return value of the pattern match (alt_t).
    -- So we substitute each argument in one by one to the alt term.
    . foldr subst alt_t
    -- When we substitute an constructor argument, 
    -- it needs to not be affected by the substitution of later arguments.
    -- So we lift their indices a number of times
    -- depending on their position in the order of substitution, 
    -- viz. those substituted first are lifted the most.
    $ zipWith liftMany [0..] args
caseOfCon _ = Fail.here


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Fail.Can m => Term -> m Term
caseApp (App (Case ind t alts) args) =
  return (Case ind t (map appArg alts))
  where
  appArg (Alt bs alt_t) =
    Alt bs (app alt_t (Indices.liftMany (length bs) args))
    
caseApp _ = Fail.here


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
appCase :: Fail.Can m => Term -> m Term
appCase term@(App _ args) = do
  cse_t <- Fail.fromMaybe (find isCase args)
  return (Term.applyCase cse_t term)
appCase _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Fail.Can m => Term -> m Term
caseCase outer_cse@(Case _ inner_cse@(Case {}) _) =
  return (Term.applyCase inner_cse outer_cse)
caseCase _ = Fail.here

