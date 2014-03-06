-- | The most basic and well established simplification steps,
-- plus a couple of functions which strongly rely on evaluation.
module Elea.Evaluation 
(
  run, steps,
  strictTerms,
  degenerateContext,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Terms as Term
import qualified Elea.Env as Env
import qualified Elea.Context as Context
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
  , constantCase
  ]
  
  
-- | The variables or uninterpreted function application terms
-- whose value must be known in order to evaluate the given term.
strictTerms :: Term -> Set Term
strictTerms (Case _ cse_t@(App (Fix {}) _) _) = 
  strictTerms cse_t
strictTerms (App (Fix _ _ fix_t) args) = id
  . collectTerms 
  $ run (App fix_t' args)
  where
  fix_t' = substAt 0 (Var Indices.omega) fix_t

  collectTerms (Case _ cse_t alts)
    | isVar (leftmost cse_t) = 
      Set.insert cse_t (concatMap altVars alts)
    where
    altVars (Alt bs alt_t) = id
      . Set.map (Indices.lowerMany (length bs))
      . Set.filter (Indices.lowerableBy (length bs))
      $ collectTerms alt_t 
  collectTerms _ = Set.empty
  
strictTerms _ = 
  Set.empty
  
    
-- | Whether a context is degenerate down the current branch of a term.
-- A degenerate context is one which has lost its shape because we are down
-- a base case branch. For example @take n _@ is degenerate when @n = 0@.
-- This function is here because it requires 'strictVars'. 
degenerateContext :: Env.MatchRead m => Context -> m Bool
degenerateContext ctx = id
  . anyM Env.isBaseCase 
  . toList 
  . strictTerms
  $ Context.apply ctx (Var Indices.omega)
    

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


-- | Removes a pattern match if every branch returns the same value.
constantCase :: forall m . Fail.Can m => Term -> m Term
constantCase (Case _ _ alts) = do
  (alt_t:alt_ts) <- mapM loweredAltTerm alts
  Fail.unless (all (== alt_t) alt_ts)
  return alt_t
  where
  loweredAltTerm :: Alt -> m Term
  loweredAltTerm (Alt bs alt_t) = 
    Indices.tryLowerMany (length bs) alt_t
    
constantCase _ = Fail.here

