module Elea.Transform.Prover
(
  Step,
  steps,
  run,
  check,
)
where


import Elea.Prelude hiding ( negate )
import Elea.Term hiding ( constructor )
import Elea.Show ( showM )
import Elea.Unification ( Unifier )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Transform.Names as Name
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Names as Name
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Fedd as Fedd  

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Poset as Partial

type Env m = 
  ( Simp.Env m
  , Env.All m
  , Fusion.Env m
  , Memo.Can m )

type Step m = ( Env m , Simp.Step m )

steps :: Step m => [Term -> m Term]
steps = 
  [ const Fail.here   
  , reflexivity
  , bottom
  , implication
  , doubleNeg
  , forAll
  , removeForAll
  , leftTrans
  , rightTrans
  , caseSplitInc
  , caseSplitDec
  , constructor
  , lfp
  , unfoldProductive
  , leqMatch
 -- , generalise
  , absurdBranch
  ]

-- | Theorem prover without fusion; use Fusion.run for the full prover  
run :: Env m => Term -> m Term
run = Transform.fix (Transform.compose all_steps)
  where
  all_steps = []
    ++ Eval.transformSteps 
    ++ Eval.traverseSteps     
    ++ Simp.steps
    ++ steps 
    
check :: (Fail.Can m, Transform.Step m) => Term -> m ()
check prop_t = do
  prop_t' <- Transform.continue prop_t
  Fail.unless (prop_t' == Term.truth)
  
reflexivity :: Fail.Can m => Term -> m Term
reflexivity (Leq x y) 
  | x == y = return truth
reflexivity _ = Fail.here


bottom :: Fail.Can m => Term -> m Term
bottom (Leq (Bot _) _) = return truth
bottom (Leq x (Bot _)) 
  | isCon (leftmost x) = return falsity
bottom _ = Fail.here


implication :: Fail.Can m => Term -> m Term
implication (Leq x y)
  | x == truth = return truth
  -- ^ anything implies true
  | y == truth = return falsity
  -- ^ true does not imply false
implication _  = Fail.here


doubleNeg :: Step m => Term -> m Term
doubleNeg (Leq x (Leq y z)) 
  | x == falsity
  , y == falsity = 
    Transform.continue z
doubleNeg _ = Fail.here


constructor :: Step m => Term -> m Term
constructor (Leq (flattenApp -> Con tc : xs) (flattenApp -> Con tc' : xs'))
  | Tag.untag tc /= Tag.untag tc' = return falsity
  | otherwise = id
    . Transform.continue 
    . conj 
    $ zipWith Leq xs xs'
constructor _ = Fail.here


unfoldProductive :: Step m => Term -> m Term
unfoldProductive leq@(Leq con_t (flattenApp -> fix@(Fix {}) : args)) 
  | isCon (leftmost con_t)
  , Term.isProductive fix = do
    History.check Name.UnfoldProductive leq 
      . Transform.continue
      $ Leq con_t (Term.reduce (Term.unfoldFix fix) args)
unfoldProductive leq@(Leq (flattenApp -> fix@(Fix {}) : args) con_t)
  | isCon (leftmost con_t) 
  , Term.isProductive fix = do
    History.check Name.UnfoldProductive leq
      . Transform.continue
      $ Leq (Term.reduce (Term.unfoldFix fix) args) con_t
unfoldProductive _ = 
  Fail.here

  
leftTrans :: Step m => Term -> m Term
leftTrans leq@(Leq x y) = 
  History.check Name.LeftTrans leq $ do
    x' <- Transform.continue x  
    Fail.when (x' == x)
    Transform.continue (Leq x' y)
leftTrans _ = Fail.here
  

rightTrans :: Step m => Term -> m Term
rightTrans leq@(Leq x y) = do
  History.check Name.RightTrans leq $ do
    y' <- Direction.invert (Transform.continue y)
    Fail.when (y' == y)
    Transform.continue (Leq x y')
rightTrans _ = Fail.here


forAll :: Step m => Term -> m Term
forAll leq@(Leq x y) = do
  Fail.unless (isLam x || isLam y)
  Transform.continue (Lam b leq')
  where
  (leq', b) 
    | Lam b x' <- x = (Leq x' (Term.reduce (Indices.lift y) [Var 0]), b)
    | Lam b y' <- y = (Leq (app (Indices.lift x) [Var 0]) y',         b)
forAll _ = Fail.here


-- | forall x . tt == tt, forall x . ff == ff
removeForAll :: Step m => Term -> m Term 
removeForAll (Lam _ t)
  | t == Term.truth = return t
  | t == Term.falsity = return t
removeForAll _ = Fail.here
    

lfp :: forall m . Step m => Term -> m Term
lfp (Leq x y) = do
  Direction.requireInc
  Fail.unless (isFixPromoted x)
  Fail.when (Unifier.exists y x)
  History.check Name.LFP (Leq x y) $ do
    let x' = fixInduction x y
    from_s <- showM (Leq x y)
    to_s <- showM (Leq x' y)
    leq' <- id
      . tracE [("leq from", from_s), ("leq to", to_s)] 
      $ Transform.continue (Leq x' y)
    return leq'
  where
  -- > fixInduction ((fix F) x) y = F (\x -> y)
  fixInduction :: Term -> Term -> Term
  fixInduction (flattenApp -> Fix _ _ fix_t : xs) y = id
    . (\f -> Term.reduce f xs)
    . Indices.subst (Term.abstractVars x_bs x_vars y)
    $ fix_t
    where
    x_vars = map fromVar xs
    x_bs = fst (flattenLam fix_t)
    
lfp _ = Fail.here


caseSplitInc :: Step m => Term -> m Term
caseSplitInc leq@(Leq (Case cse_t alts) y) = do
  Direction.requireInc
  History.check Name.CaseSplit leq $ do
    let leq' = Case cse_t (map leqAlt alts)
    Transform.continue leq'
  where
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq alt_t y')
    where
    y' = Indices.liftMany (nlength bs) y
    
caseSplitInc leq@(Leq left_t (Case cse_t@(Var x) alts)) = do
  Direction.requireInc
  Fail.unless (x `Indices.freeWithin` left_t)
  x_ty <- Type.getM cse_t
  left_t_bot <- Simp.runM (Indices.replaceAt x (Bot x_ty) left_t)
  Fail.unless (isBot left_t_bot)
  History.check Name.CaseSplit leq $ do
    let leq' = Case cse_t (map leqAlt alts)
    Transform.continue leq'
  where 
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq left_t' alt_t)
    where
    left_t' = Indices.liftMany (nlength bs) left_t 
    
caseSplitInc _ = Fail.here

caseSplitDec :: Step m => Term -> m Term
caseSplitDec leq@(Leq x (Case cse_t alts)) = do
  Direction.requireDec
  History.check Name.CaseSplit leq $ do
    let leq' = Case cse_t (map leqAlt alts)
    Transform.continue leq'
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
  return (Case cse_t (map mkAlt alts))
  where
  mkAlt alt@(Alt tc bs alt_t) = 
    Alt tc bs (Leq pat_t alt_t)
    where
    pat_t = patternTerm (altPattern alt)
leqMatch _ = Fail.here

    
absurdBranch :: Step m => Term -> m Term
absurdBranch orig_t@(Case (Var x) alts) = do
  Direction.requireInc
  ty <- Type.getM orig_t
  Fail.unless (Type.Base Type.prop == ty)
  alts' <- zipWithM absurdAlt [0..] alts
  Fail.when (alts' == alts)
  Transform.continue (Case (Var x) alts')
  where
  absurdAlt n alt@(Alt tcon bs alt_t) 
    | Type.isBaseCase (Tag.untag tcon) = do
      is_abs <- id
        . Env.bindMany bs
        . Env.matched (Term.matchFromCase n orig_t)
        $ absurdEnv
      if is_abs 
      then do
        ty <- Type.getM orig_t
        return (Alt tcon bs (Bot ty))
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
        . Memo.memo Name.AbsurdEnv prop
        . Fusion.disable
        $ Transform.continue prop
        -- ^ Using our prover backwards 
        -- performs proof by contradiction
      return (prop' == Term.falsity)
      where 
      cse_t = Term.matchedTerm match
      pat_t = Term.matchedTo match
      prop = Leq pat_t cse_t
    
absurdBranch _ = Fail.here


generalise :: Step m => Term -> m Term
generalise leq@(Leq (flattenApp -> Fix {} : args) right_t) 
  | not (null to_gen) = do
    leq_s <- showM leq
    Transform.continue 
      $ foldr Term.tryGeneralise leq to_gen
  where 
  to_gen = id
    . filter (\t -> t `Term.isSubterm` right_t)
    . filter isFix
    $ args 
    {-
generalise leq@(Leq (flattenApp -> fix@(Fix {}) : args) right_t) 
  | not (null free_vars) = do
    leq' <- Term.tryGeneraliseInFix (head free_vars) leq
    leq_s <- showM leq'
    id
      . tracE [("gen leq", leq_s)]
      $ Transform.continue leq'
  where
  free_vars = id
    . filter (\x -> freeInFix x fix)
    . map fromVar
    . filter isVar
    $ args
  
  freeInFix :: Index -> Term -> Bool
  freeInFix var fix =
    isFix fix && Indices.freeWithin var fix
    -}
generalise _ =
  Fail.here