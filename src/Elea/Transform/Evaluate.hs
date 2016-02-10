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
import qualified Elea.Transform.Names as Name
import qualified Elea.Monad.History as History

import qualified Data.Set as Set
import qualified Data.Poset as Quasi

-- TODO absurdity needs to properly evaluate strictness of arguments
-- TODO traverses should be successively applied to 
  -- the tallest branch downwards
-- TODO SPECIALISE  
-- TODO equality checking after traverse steps is ridiculous

type Step m = (Env m, Transform.Step m)

type Env m = 
  ( Env.Write m
  , Defs.Read m
  , History.Env m
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
  [ Transform.step "normalise app" normaliseApp
  , Transform.step "propagate _|_" strictness
  , Transform.step "beta reduction" beta
  , Transform.step "case-of reduction" caseOfCon
  , Transform.step "commute case-app" caseApp
  , Transform.step "commute app-case" appCase
  , Transform.step "commute case-case" caseCase
  , Transform.step "seq reduction" reduceSeq
  , Transform.step "clean fix" cleanFix ]

traverseSteps :: Env m => [Transform.NamedStep m]
traverseSteps = 
  [ Transform.step "traverse match" traverseMatch
  , Transform.step "traverse branches" traverseBranches
  , Transform.step "traverse fun" traverseFun
  , Transform.step "traverse app" traverseApp
  , Transform.step "traverse fix" traverseFix ]
  
unwrapDepth :: Nat
unwrapDepth = 2
  
  
-- | Finds terms that are undefined and sets them that way.
-- So far it detects applying arguments to an absurd function.
-- Need to add pattern matching over absurdity, but how to find the type?
strictness :: Step m => Term -> m Term
strictness term 
  | isUndef term = do
    Transform.noMoreRewrites
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
beta t@(App f@(Lam _ rhs) xs) = id
  . History.check Name.Beta t
  . return
  $ Term.reduce f xs
beta _ = Fail.here


caseOfCon :: Step m => Term -> m Term
caseOfCon term@(Case cse_t alts)
  | (isCon . leftmost) cse_t = id
    . History.check Name.CaseOfCon term
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
traverseMatch term@(Case cse_t alts) = 
  History.check Name.TraverseMatch term $ do 
    cse_t' <- Transform.traverse (\t -> Case t alts) cse_t  
    Fail.when (cse_t == cse_t')
    return (Case cse_t' alts)
traverseMatch _ = Fail.here


traverseBranches :: forall m . Step m => Term -> m Term
traverseBranches term@(Case cse_t alts) = 
  History.check Name.TraverseBranch term $ do
    alts' <- zipWithM traverseAlt [0..] alts
    let term' = Case cse_t alts'
    Fail.when (term == term')
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


traverseFun :: Step m => Term -> m Term
traverseFun (Lam b t) = do
  t' <- id
    . Env.bind b 
    $ Transform.traverse (\t -> Lam b t) t
  Fail.when (t == t')
  Transform.noMoreRewrites
  if t' == Term.truth || t' == Term.falsity
  then return t'
  else return (Lam b t')
traverseFun _ = Fail.here


traverseApp :: forall m . Step m => Term -> m Term
traverseApp term@(App f xs) = 
  History.check Name.TraverseApp term $ do
    xs' <- mapM traverseArg (range xs)
    f' <- id
      $ Transform.traverse (\f -> App f xs) f
    let term' = App f' xs'
    Fail.when (term == term')
    return term'
  where
  traverseArg :: Nat -> m Term
  traverseArg n = Transform.traverse (\t -> App f (setAt (enum n) t xs)) (xs !! n)
  
traverseApp _ = Fail.here


traverseFix :: Step m => Term -> m Term
traverseFix fix@(Fix inf b t) = do
  t' <- id
    . Env.bind b
    $ Transform.traverse (\t -> Fix inf b t) t
  Fail.when (t == t')
  Transform.noMoreRewrites
  return
    . Term.dirtyFix
    $ Fix inf b t'
    
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



-- | If we have a case statement on the left of term 'App'lication
-- then float it out.
caseApp :: Step m => Term -> m Term
caseApp (App (Case t alts) args) =
  return (Case t (map appArg alts))
  where
  appArg (Alt con bs alt_t) =
    Alt con bs (app alt_t (Indices.liftMany (nlength bs) args))
    
caseApp _ = Fail.here


reduceSeq :: Step m => Term -> m Term
reduceSeq (Seq (Bot _) t) = do
  Transform.noMoreRewrites
  return (Bot (Type.get t))
reduceSeq (Seq (leftmost -> Con {}) t) =
  return t
reduceSeq _ = Fail.here


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


cleanFix :: Step m => Term -> m Term
cleanFix fix_t@Fix{ fixInfo = fix_info, inner = fix_body } 
  | get fixIsDirty fix_info = do
    mby_name <- (liftM (map fst) . Defs.lookupName) fix_t
    let fix_info' = id
          . set fixName mby_name 
          . set fixIsDirty False
          . set fixIsClosed (Set.null (Term.freeVarSet fix_t))
          $ fix_info
    Transform.traverse id (fix_t { fixInfo = fix_info' })
    -- ^ traverse makes the step invisible in tracing
cleanFix _ = 
  Fail.here
