-- | The most basic and well established simplification steps,
-- plus a couple of functions which strongly rely on evaluation.
module Elea.Evaluation 
(
  run, steps,
  strictTerms,
  degenerateContext,
  floatVarMatches,
)
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Terms as Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set

run :: Term -> Term                           
run = Fold.rewriteSteps steps

steps :: Fail.Can m => [Term -> m Term]
steps = 
  [ normaliseApp
  , eta 
  , absurdity
  , beta
  , caseOfCon
  , caseApp
  , appCase
  , caseCase
  ]
  
unwrapDepth :: Nat
unwrapDepth = 2
  
-- | The variables or uninterpreted function application terms
-- whose value must be known in order to evaluate the given term.
strictTerms :: Term -> Set Term
strictTerms (Case cse_t@(App (Fix {}) _) _) = 
  strictTerms cse_t
strictTerms (App fix@(Fix _ _ fix_t) args) = id
  . collectTerms 
  $ run (App fix_t' args)
  where
  fix_t' = Term.unwrapFix unwrapDepth fix

  collectTerms (Case cse_t alts)
    | isVar (leftmost cse_t) = Set.insert cse_t alt_terms
    | otherwise = alt_terms
    where
    alt_terms = concatMap altVars alts
      where
      altVars (Alt _ bs alt_t) = id
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
    
  
-- | Finds terms that are absurd and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
absurdity :: Fail.Can m => Term -> m Term
absurdity term
  | Type.has term
  , absurd term = 
    return (Unr (Type.get term))
  where 
  absurd (App (Unr _) _) = True
  absurd (App _ args) = any Term.isUnr args
  absurd (Case (Unr _) _) = True
  absurd _ = False
absurdity _ =
  Fail.here
  
  
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
caseOfCon (Case cse_t alts)
  | Con con : args <- flattenApp cse_t
  , Alt _ bs alt_t <- alts !! get Type.constructorIndex con = id
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
caseApp (App (Case t alts) args) =
  return (Case t (map appArg alts))
  where
  appArg (Alt con bs alt_t) =
    Alt con bs (app alt_t (Indices.liftMany (length bs) args))
    
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
caseCase outer_cse@(Case inner_cse@(Case {}) _) =
  return (Term.applyCase inner_cse outer_cse)
caseCase _ = Fail.here


-- | Moves all pattern matches over variables topmost in a term. 
-- Be careful with this, it can cause loops if combined with all sorts 
-- of things.
floatVarMatches :: Term -> Term
floatVarMatches = id
  -- Then, we recurse down the top level of pattern matches,
  -- which the 'Term.recursionScheme' isomorphism restricts us to,
  -- and we float all matches over variables to the top
  . Fold.isoRewrite Term.recursionScheme float 
  -- First we run evaluation
  . run
  where
  float :: forall m . Fail.Can m => Term -> m Term
  float outer_t@(Case (leftmost -> Fix {}) alts) = do
    inner_case <- Fail.choose (map caseOfVarAlt alts)
    return  
      . run
      $ Term.applyCase inner_case outer_t
    where
    caseOfVarAlt :: Alt -> m Term
    -- We return the inner alt case-of if it is over a variable
    -- which is not from the pattern match, viz. it can be lowered
    -- to outside the match.
    caseOfVarAlt (Alt _ bs alt_t@(Case cse_t i_alts)) 
      | isVar cse_t = do
        cse_t' <- Indices.tryLowerMany (length bs) cse_t
        return (Case cse_t' i_alts)
    caseOfVarAlt _ = 
      Fail.here
      
  float _ = Fail.here


