-- | The most basic and well established simplification steps,
-- plus a couple of functions which strongly rely on evaluation.
module Elea.Transform.Evaluate 
(
  Step, Env,
  apply, 
  transformSteps, 
  traverseSteps,
  --floatVarMatches,
  caseOfCon,
)
where

import Elea.Prelude
import Elea.Term.Index
import Elea.Term
import qualified Elea.Type as Type
import qualified Elea.Term.Ext as Term
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Term.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Foldable.WellFormed as WellFormed
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Transform.Signals as Signals
import qualified Elea.Transform.Names as Step

import qualified Data.Set as Set
import qualified Data.Poset as Quasi

-- TODO am missing the bot arg of strict function case in strictness step

type Step m = (Env m, Transform.Step m)

type Env m = 
  ( Env.Write m
  , Defs.Read m
  , Transform.Env m )

apply :: Term -> Term
apply = id
  . Fedd.eval
  . Transform.compose all_steps
  . WellFormed.check
  where
  all_steps = []
    ++ transformSteps 
    ++ traverseSteps
  
{-# SPECIALISE transformSteps :: [Transform.NamedStep (Fedd.FeddT IO)] #-}
{-# SPECIALISE traverseSteps :: [Transform.NamedStep (Fedd.FeddT IO)] #-}
{-# INLINEABLE transformSteps #-}  
{-# INLINEABLE traverseSteps #-}

transformSteps :: Env m => [Transform.NamedStep m]
transformSteps =
  [ Transform.step Step.NormaliseApp normaliseApp
  , Transform.step Step.Strictness strictness
  , Transform.step Step.BetaReduce beta
  , Transform.step Step.CaseOfCon caseOfCon
  , Transform.step Step.CaseAsFun caseApp
  , Transform.step Step.CaseAsArg appCase
  , Transform.step Step.CommuteCases caseCase ]

traverseSteps :: Env m => [Transform.NamedStep m]
traverseSteps = 
  [ Transform.silentStep Step.TraverseMatch traverseMatch
  , Transform.silentStep Step.TraverseBranches traverseBranches
  , Transform.silentStep Step.TraverseLam traverseLam
  , Transform.silentStep Step.TraverseApp traverseApp
  , Transform.silentStep Step.TraverseFix traverseFix ]

  
-- | Finds terms that are undefined and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
strictness :: Step m => Term -> m Term
strictness (Seq (Bot _) t) = do
  Signals.tellStopRewriting
  return (Bot (Type.get t))
strictness (Seq (leftmost -> Con {}) t) =
  return t
strictness term 
  | isUndef term = do
    Signals.tellStopRewriting
    return (Bot (Type.get term))
  where 
  isUndef (App (Bot _) _) = True
  isUndef (Case (Bot _) _) = True
  isUndef (Case _ alts)
    | all (isBot . get altInner) alts = True
  isUndef _ = False
strictness _ =
  Fail.here
  
  
normaliseApp :: Step m => Term -> m Term
normaliseApp (App f []) = 
  return f
normaliseApp (App (App f ts1) ts2) = 
  return (App f (ts1 ++ ts2))
normaliseApp _ = 
  Fail.here
  

beta :: Step m => Term -> m Term
beta t@(App f@(Lam _ rhs) xs) =
  return (Term.reduce f xs)
beta _ = Fail.here


caseOfCon :: Step m => Term -> m Term
caseOfCon term@(Case cse_t alts)
  | (isCon . leftmost) cse_t = id
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
  where
  Con tcon : args = flattenApp cse_t
  Alt _ bs alt_t = id
    . (alts !!)
    . get Type.constructorIndex
    $ Tag.untag tcon
    
    
caseOfCon _ = Fail.here


traverseMatch :: Step m => Term -> m Term
traverseMatch term@(Case cse_t alts) = do 
  (cse_t', signals) <- id
    . Signals.listen
    $ Transform.traverse (\t -> Case t alts) cse_t  
  Fail.unless (get Signals.didRewrite signals)
  return (Case cse_t' alts)
traverseMatch _ = Fail.here


traverseBranches :: forall m . Step m => Term -> m Term
traverseBranches term@(Case cse_t alts) = do
  (alts', signals) <- id
    . Signals.listen
    $ zipWithM traverseAlt [0..] alts
  Fail.unless (get Signals.didRewrite signals)
  return (Case cse_t alts')
  where
  traverseAlt n alt@(Alt con bs t) = do
    t' <- id
      . Env.bindMany bs
      . Env.matched (Term.matchFromCase n term)
      . Transform.traverse altContext 
      $ substituteVar t
    return (Alt con bs t')
    where
    cse_t_here = Indices.liftMany (nlength bs) cse_t
    pat_t = (patternTerm . altPattern) alt

    substituteVar 
      | Var { varIndex = var } <- cse_t_here = Indices.replaceAt var pat_t
      | otherwise = id

    altContext gap_t = Case cse_t ctx_alts
      where
      ctx_alts = setAt (enum n) (Alt con bs gap_t) alts
traverseBranches _ = Fail.here


traverseLam :: Step m => Term -> m Term
traverseLam (Lam b t) = do
  (t', signals) <- id
    . Signals.listen
    . Env.bind b 
    $ Transform.traverse (\t -> Lam b t) t
  Fail.unless (get Signals.didRewrite signals)
  Signals.tellStopRewriting
  if t' == Term.truth || t' == Term.falsity
  then return t'
  else return (Lam b t')
traverseLam _ = Fail.here


traverseApp :: forall m . Step m => Term -> m Term
traverseApp term@(App f xs) = do
  (xs', xs_signals) <- id
    . Signals.listen
    $ mapM traverseArg (range xs)
  (f', f_signals) <- id
    . Signals.listen
    $ Transform.traverse (\f -> App f xs) f
  Fail.unless (get Signals.didRewrite (xs_signals ++ f_signals))
  return (App f' xs')
  where
  traverseArg :: Nat -> m Term
  traverseArg n = Transform.traverse (\t -> App f (setAt (enum n) t xs)) (xs !! n)
  
traverseApp _ = Fail.here


traverseFix :: Step m => Term -> m Term
traverseFix fix@(Fix inf b t) = do
  (t', signals) <- id
    . Signals.listen
    . Env.bind b
    $ Transform.traverse (\t -> Fix inf b t) t
  Fail.unless (get Signals.didRewrite signals)
  Signals.tellStopRewriting
  Term.lookupFixName (Fix inf b t')
    
traverseFix _ = Fail.here


-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Step m => Term -> m Term
caseApp (App (Case t alts) args) =
  return (Case t (map appArg alts))
  where
  appArg (Alt con bs alt_t) =
    Alt con bs (app alt_t (Indices.liftMany (nlength bs) args))
    
caseApp _ = Fail.here


-- | If we have a case statement on the right of term 'App'lication
-- then float it out.
appCase :: Step m => Term -> m Term
appCase term@(App f@(Fix {}) xs) = do
  cse_i <- Fail.fromMaybe (findIndex isCase xs)
  return (applyCase cse_i)
  where
  applyCase cse_i = 
    Case cse_t (map applyAlt alts)
    where
    Case cse_t alts = xs !! cse_i
    
    applyAlt (Alt con bs alt_t) = 
      Alt con bs (app f' xs')
      where
      f' = Indices.liftMany (nlength bs) f
      xs' = id 
        . setAt cse_i alt_t
        $ Indices.liftMany (nlength bs) xs
      
appCase _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Step m => Term -> m Term
caseCase outer_cse@(Case inner_cse@(Case inner_t inner_alts) outer_alts) =
  return (Case inner_t (map newOuterAlt inner_alts))
  where
  newOuterAlt :: Alt -> Alt
  newOuterAlt (Alt con bs t) = 
    Alt con bs (Case t alts_here)
    where
    alts_here = map (Indices.liftMany (nlength bs)) outer_alts
caseCase _ = Fail.here
