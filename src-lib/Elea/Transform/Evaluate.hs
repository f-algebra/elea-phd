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
  --floatVarMatches,
  
  caseOfCon,
)
where

import Elea.Prelude
import Elea.Term.Index
import Elea.Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Term.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Height as Height
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Transform.Names as Name
import qualified Elea.Monad.History as History

import qualified Data.Set as Set
import qualified Data.Poset as Partial

-- TODO absurdity needs to properly evaluate strictness of arguments
-- TODO traverses should be successively applied to 
  -- the tallest branch downwards
  
-- TODO URGENT remove var-branch and just use term-branch??
-- TODO strictness needs Type.getM

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
  , strictness
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
  
  
reduce :: Term -> [Term] -> Term
reduce (Lam _ rhs) (x:xs) = 
  reduce (subst x rhs) xs
reduce f xs = 
  app f xs
    
  
-- | Finds terms that are undefined and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
strictness :: Step m => Term -> m Term
strictness term 
  | Type.has term
  , evalsToBot term = 
    return (Bot (Type.get term))
  where 
  evalsToBot t | Term.isBot t = True
  evalsToBot (App f xs) = evalsToBot f || any evalsToBot xs
  evalsToBot (Case t _) = evalsToBot t
  evalsToBot _ = False
strictness _ =
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
  . History.check Name.Beta t
  . Transform.continue
  $ reduce f xs
beta _ = Fail.here


caseOfCon :: Step m => Term -> m Term
caseOfCon term@(Case cse_t alts)
  | Con con : args <- flattenApp cse_t
  , Alt _ bs alt_t <- alts !! get Type.constructorIndex con = id
    . History.check Name.CaseOfCon term
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
traverseMatch term@(Case cse_t alts) =
  History.check Name.TraverseMatch cse_t $ do
    cse_t' <- Transform.continue cse_t
    Fail.when (cse_t == cse_t')
    Transform.continue (Case cse_t' alts)
traverseMatch _ = Fail.here


traverseBranches :: Step m => Term -> m Term

traverseBranches term@(Case (Var x) alts) =
  History.check Name.TraverseVarBranch term $ do
    alts' <- mapM traverseAlt alts
    Fail.when (alts' == alts)
    Transform.continue (Case (Var x) alts')
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
    
traverseBranches term@(Case cse_t alts) = 
  History.check Name.TraverseBranch term $ do
    alts' <- mapM traverseAlt alts
    Fail.when (alts' == alts)
    Transform.continue (Case cse_t alts')
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
traverseApp term@(App f xs) = 
  History.check Name.TraverseApp term $ do
    xs' <- mapM Transform.continue xs
    f' <- Transform.continue f
    let term' = App f' xs'
    Fail.when (term' == term)
    Transform.continue term'
  
traverseApp _ = Fail.here


traverseFix :: Step m => Term -> m Term
traverseFix (Fix inf b t) = do
  t' <- id
    . Env.bind b
    $ Transform.continue t
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

