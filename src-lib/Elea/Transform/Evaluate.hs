-- | The most basic and well established simplification steps,
-- plus a couple of functions which strongly rely on evaluation.
module Elea.Transform.Evaluate 
(
  Step,
  run, 
  reduce,
  transformSteps, 
  traverseSteps,
  strictTerms,
  degenerateContext,
  --floatVarMatches,
  
  caseOfCon,
)
where

import Elea.Prelude
import Elea.Term.Index
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Term.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Height as Height
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.History as History

import qualified Data.Set as Set
import qualified Data.Poset as Partial

-- TODO absurdity needs to properly evaluate strictness of arguments
-- TODO traverses should be successively applied to 
  -- the tallest branch downwards
  
-- TODO URGENT remove var-branch and just use term-branch??

type Step m = 
  ( Transform.Step m
  , Fail.Can m
  , Env.Write m
  , History.Env m )
  

run :: Term -> Term
run = flip runReader ([] :: [Bind]) 
    . History.emptyEnvT
    -- ^ Use the Reader [Bind] instance for type environments
    . Transform.fix (Transform.compose all_steps)
  where
  all_steps = transformSteps ++ traverseSteps
  
  
transformSteps :: Step m => [Term -> m Term]
transformSteps =
  [ normaliseApp
 -- , eta 
  , absurdity
  , beta
  , caseOfCon 
  ]

traverseSteps :: Step m => [Term -> m Term]
traverseSteps = 
  [ traverseMatch
  , traverseBranches
  , traverseFun
  , traverseApp
  , traverseFix
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
  
reduce :: Term -> [Term] -> Term
reduce (Lam _ rhs) (x:xs) = 
  reduce (subst x rhs) xs
reduce f xs = 
  app f xs
    
  
-- | Finds terms that are absurd and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
absurdity :: Step m => Term -> m Term
absurdity (Unr _) = 
  Fail.here
  -- ^ Cannot simplify further
absurdity term
  | Type.has term
  , absurd term = 
    Transform.continue (Unr (Type.get term))
  where 
  absurd (Unr _) = True
  absurd (App f xs) = absurd f || any absurd xs
  absurd (Case t _) = absurd t
  absurd _ = False
absurdity _ =
  Fail.here
  
  
normaliseApp :: Step m => Term -> m Term
normaliseApp (App f []) = 
  Transform.continue f
normaliseApp (App (App f ts1) ts2) = 
  Transform.continue (App f (ts1 ++ ts2))
normaliseApp _ = 
  Fail.here
  

beta :: Step m => Term -> m Term
beta t@(App f@(Lam _ rhs) xs) = id
  . History.check "beta" t
  . Transform.continue
  $ reduce f xs
beta _ = Fail.here
  

eta :: Step m => Term -> m Term
eta (Lam _ (App f xs@(last -> Var 0)))
  | not (0 `Set.member` Indices.free new_t) = 
    Transform.continue (Indices.lower new_t)
  where
  new_t = app f (init xs)
eta _ = Fail.here


caseOfCon :: Step m => Term -> m Term
caseOfCon (Case cse_t alts)
  | Con con : args <- flattenApp cse_t
  , Alt _ bs alt_t <- alts !! get Type.constructorIndex con = id
    . Transform.continue
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


traverseMatch :: Step m => Term -> m Term
traverseMatch term@(Case cse_t alts) = do
  cse_t' <- Transform.continue cse_t
  Fail.when (cse_t' == cse_t)
  if True || (Case cse_t' []) Partial.< (Case cse_t [])
  then Transform.continue (Case cse_t' alts)
      -- ^ Only recurse if we have shrunk our term
      -- removing the branches is a slight speed optimisation
  else return (Case cse_t' alts)
traverseMatch _ = Fail.here


traverseBranches :: Step m => Term -> m Term

traverseBranches term@(Case (Var x) alts) = do
  alts' <- mapM traverseAlt alts
  Fail.when (alts' == alts)
  let term' = Case (Var x) alts'
  if term' Partial.< term
  then Transform.continue term'
  else return (Case (Var x) alts')
  where
  traverseAlt (Alt con bs t) = do
    t' <- id
      . Env.bindMany bs
      . Transform.continue 
      -- Substitute the variable we have just bound for the 
      -- pattern it has been bound to
      $ Indices.replaceAt x_here pat_t t
    return (Alt con bs t')
    where
    x_here = Indices.liftMany (length bs) x
    pat_t = altPattern con
    
traverseBranches term@(Case cse_t alts) = do
  alts' <- mapM traverseAlt alts
  Fail.when (alts' == alts)
  let term' = Case cse_t alts'
  if True || term' Partial.< term
  then Transform.continue term'
  else return term'
  where
  traverseAlt (Alt con bs t) = do
    t' <- id
      . Env.bindMany bs
      . Env.matched cse_t_here pat_t
      $ Transform.continue t
    return (Alt con bs t')
    where
    cse_t_here = Indices.liftMany (length bs) cse_t
    pat_t = altPattern con
    
traverseBranches _ = Fail.here


traverseFun :: Step m => Term -> m Term
traverseFun (Lam b t) = do
  t' <- Env.bind b (Transform.continue t)
  Fail.when (t' == t)
  return (Lam b t')
traverseFun _ = Fail.here


traverseApp :: Step m => Term -> m Term
traverseApp term@(App f xs) = do
  xs' <- mapM Transform.continue xs
  f' <- Transform.continue f
  let term' = App f' xs'
  Fail.when (term' == term)
  if True || term' Partial.< term
  then Transform.continue term'
    -- ^ Only recurse if we have shrunk the term
  else return term'
  
traverseApp _ = Fail.here


traverseFix :: Step m => Term -> m Term
traverseFix (Fix inf b t) = do
 -- Fail.when (get fixClosed inf)
  t' <- id
    . Env.bind b
    . History.check "fix" t
    $ Transform.continue t
 -- let inf' = set fixClosed True inf
  Fail.when (t' == t)
  return (Fix inf b t')
traverseFix _ = Fail.here


{-
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
-}
