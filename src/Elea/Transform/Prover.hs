module Elea.Transform.Prover
(
  Step, Env,
  steps,
  applyM,
)
where


import Elea.Prelude hiding ( negate, run )
import Elea.Term hiding ( constructor )
import Elea.Unification ( Unifier )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Type as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Transform.Names as Step
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable.WellFormed as WellFormed
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Fedd as Fedd  
import qualified Elea.Monad.Transform.Signals as Signals
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Poset as Partial

type Env m = 
  ( Simp.Env m
  , Memo.Can m )

type Step m = ( Env m , Simp.Step m )

{-# SPECIALISE steps :: [Transform.NamedStep (Fedd.FeddT IO)] #-}
{-# INLINEABLE steps #-}  

steps :: Env m => [Transform.NamedStep m]
steps = 
  [ Transform.step Step.LeqReflexive reflexivity
  , Transform.step Step.BotLeq bottom
  , Transform.step Step.DoubleNegation doubleNeg
  , Transform.step Step.ForAll forAll
  , Transform.step Step.UnusedForAll removeForAll
  , Transform.silentStep Step.LeqLeftTrans leftTrans
  , Transform.silentStep Step.LeqRightTrans rightTrans
  , Transform.step Step.CaseSplitInc caseSplitInc
  , Transform.step Step.CaseSplitDec caseSplitDec
  , Transform.step Step.LeqConstructor constructor
  , Transform.step Step.LeastFixedPoint lfp
  , Transform.step Step.LeqProductive unfoldProductive
  , Transform.step Step.CaseOfLeq leqMatch
  , Transform.step Step.AbsurdBranch absurdBranch ]

-- | Theorem prover without fusion; use Fusion.run for the full prover  
applyM :: Env m => Term -> m Term
applyM = id
  . Transform.clearContext
  . Transform.compose all_steps
  . WellFormed.check
  where
  all_steps = []
    ++ Eval.transformSteps 
    ++ Eval.traverseSteps     
    ++ Simp.steps
    ++ steps 
    
reflexivity :: Step m => Term -> m Term
reflexivity (Leq x y) 
  | x == y = return truth
reflexivity _ = Fail.here


bottom :: Step m => Term -> m Term
bottom (Leq (Bot _) _) = return truth
bottom (Leq x (Bot _)) 
  | isCon (leftmost x) = return falsity
bottom _ = Fail.here


doubleNeg :: Step m => Term -> m Term
doubleNeg (Leq x (Leq y z)) 
  | x == falsity
  , y == falsity = 
    return z
doubleNeg _ = Fail.here


constructor :: Step m => Term -> m Term
constructor (Leq (flattenApp -> Con tc : xs) (flattenApp -> Con tc' : xs'))
  | Tag.untag tc /= Tag.untag tc' = return falsity
  | otherwise = id
    . return 
    . conj 
    $ zipWith Leq xs xs'
constructor _ = Fail.here


unfoldProductive :: Step m => Term -> m Term
unfoldProductive leq@(Leq con_t (flattenApp -> fix@(Fix {}) : args)) 
  | isCon (leftmost con_t)
  , Term.isProductive fix = id
    . return
    $ Leq con_t (Term.reduce (Term.unfoldFix fix) args)
unfoldProductive leq@(Leq (flattenApp -> fix@(Fix {}) : args) con_t)
  | isCon (leftmost con_t) 
  , Term.isProductive fix = id
    . return
    $ Leq (Term.reduce (Term.unfoldFix fix) args) con_t
unfoldProductive _ = 
  Fail.here

  
leftTrans :: Step m => Term -> m Term
leftTrans leq@(Leq x y) = do
  (x', signals) <- id
    . Signals.listen
    $ Transform.traverse (\t -> Leq t y) x  
  Fail.unless (get Signals.didRewrite signals)
  return (Leq x' y)
leftTrans _ = Fail.here
  

rightTrans :: Step m => Term -> m Term
rightTrans leq@(Leq x y) = do
  (y', signals) <- id
    . Signals.listen
    . Direction.invert  
    $ Transform.traverse (\t -> Leq x t) y
  Fail.unless (get Signals.didRewrite signals)
  return (Leq x y')
rightTrans _ = Fail.here


forAll :: Step m => Term -> m Term
forAll leq@(Leq x y) = do
  Fail.unless (isLam x || isLam y)
  return (Lam b leq')
  where
  (leq', b) 
    | Lam b x' <- x = (Leq x' (Term.reduce (Indices.lift y) [Var 0 b]), b)
    | Lam b y' <- y = (Leq (app (Indices.lift x) [Var 0 b]) y',         b)
forAll _ = Fail.here


-- | forall x . tt == tt, forall x . ff == ff
removeForAll :: Step m => Term -> m Term 
removeForAll (Lam _ t)
  | t == Term.truth || t == Term.falsity = do
    Signals.tellStopRewriting
    return t
removeForAll _ = Fail.here
     

lfp :: forall m . Step m => Term -> m Term
lfp (Leq x y) = do
  Direction.requireInc
  Fail.unless (isFixPromoted x)
  Fail.when $ True
    && not (isVar y) 
    && Unifier.exists y x 
    && not (Unifier.alphaEq x y)
  return (Leq x' y)
  where
  x' = fixInduction x y

  -- > fixInduction ((fix F) x) y = F (\x -> y)
  fixInduction :: Term -> Term -> Term
  fixInduction (flattenApp -> Fix _ _ fix_t : xs) y = id
    . (\f -> Term.reduce f xs)
    . Indices.subst (Term.abstractVars xs y)
    $ fix_t
    
lfp _ = Fail.here


caseSplitInc :: Step m => Term -> m Term
caseSplitInc leq@(Leq (Case cse_t alts) y) = do
  Direction.requireInc
  return (Case cse_t (map leqAlt alts))
  where
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq alt_t y')
    where
    y' = Indices.liftMany (nlength bs) y
    
caseSplitInc leq@(Leq left_t (Case cse_t@(Var x _) alts)) = do
  Direction.requireInc
  Fail.unless (x `Indices.freeWithin` left_t)
  left_t_bot <- id
    . Simp.applyM 
    $ Indices.replaceAt x (Bot cse_ty) left_t
  Fail.unless (isBot left_t_bot)
  return (Case cse_t (map leqAlt alts))
  where 
  cse_ty = Type.get cse_t

  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq left_t' alt_t)
    where
    left_t' = Indices.liftMany (nlength bs) left_t 
    
caseSplitInc _ = Fail.here


caseSplitDec :: Step m => Term -> m Term
caseSplitDec leq@(Leq x (Case cse_t alts)) = do
  Direction.requireDec
  return (Case cse_t (map leqAlt alts))
  where
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq x' alt_t)
    where
    x' = Indices.liftMany (nlength bs) x

caseSplitDec _ = Fail.here


leqMatch :: Step m => Term -> m Term
leqMatch (Leq t (Case cse_t alts)) = do
  Direction.requireInc
  Fail.unless (t == cse_t)
  Signals.tellStopRewriting
  return (Case cse_t (map mkAlt alts))
  where
  mkAlt alt@(Alt tc bs alt_t) = 
    Alt tc bs (Leq pat_t alt_t)
    where
    pat_t = patternTerm (altPattern alt)
leqMatch _ = Fail.here

    
absurdBranch :: Step m => Term -> m Term
absurdBranch orig_t@(Case (Var x b) alts) = do
  Direction.requireInc
  Fail.unless (Type.Base Type.prop == orig_ty)
  alts' <- zipWithM absurdAlt [0..] alts
  Fail.when (alts' == alts)
  return (Case (Var x b) alts')
  where
  orig_ty = Type.get orig_t

  absurdAlt n alt@(Alt tcon bs alt_t) 
    | Type.isBaseCase (Tag.untag tcon) = do
      is_abs <- id
        . Env.bindMany bs
        . Env.matched (Term.matchFromCase n orig_t)
        $ absurdEnv
      if is_abs 
      then do
        return (Alt tcon bs (Bot orig_ty))
      else
        return alt
    | otherwise = 
      return alt

  absurdEnv :: Step m => m Bool
  absurdEnv = do
    ms <- liftM (filter potentialAbs) Env.matches
    anyM isAbsurd ms
    where
    potentialAbs match = True
      && isFix (leftmost cse_t) 
      && any Term.isFinite dec_args 
      where
      cse_t = Term.matchedTerm match
      App fix args = cse_t
      dec_args = map (args !!) (Term.decreasingArgs fix)
      
    isAbsurd match = do
      prop' <- id
        . Direction.local Direction.Dec
        . Fusion.disable
        $ Transform.restart prop
        -- ^ Using our prover backwards 
        -- performs proof by contradiction
      return (prop' == Term.falsity)
      where 
      cse_t = Term.matchedTerm match
      pat_t = Term.matchedTo match
      prop = Leq pat_t cse_t
    
absurdBranch _ = Fail.here
