module Elea.Transform.Prover
(
  steps,
  run,
)
where


import Elea.Prelude
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
import qualified Elea.Transform.Rewrite as Rewrite
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Fedd as Fedd  

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Poset as Partial

type Step m = Rewrite.Step m

steps :: Step m => [Term -> m Term]
steps = 
  [ const Fail.here   
  , reflexivity
  , bottom
  , doubleNeg
  , forAll
  , caseSplit
  , constructor
  , rewriteEq
  , rewriteLeq
  , lfp
  ]

-- | Theorem prover without fusion; use Fusion.run for the full prover  
run :: Term -> Term
run = id
  . Fedd.eval
  . Transform.fix (Transform.compose all_steps)
  where
  all_steps = []
    ++ Eval.transformSteps 
    ++ Eval.traverseSteps     
    ++ Simp.steps
    ++ steps 
    
equivSteps :: Step m => [Term -> m Term]
equivSteps =  
  [ reflexivity
  , bottom
  , doubleNeg
  , forAll
  , constructor
  , rewriteEq 
  ]
  
runEquiv :: Step m => Term -> m Term
runEquiv = 
  Transform.fix (Transform.compose all_steps)
  where
  all_steps = []
    ++ Eval.transformSteps 
    ++ Eval.traverseSteps
    ++ Simp.equivSteps
    ++ Rewrite.equivSteps
    ++ equivSteps
    
  
implies :: Term -> Term -> Term
implies = flip Leq
    
neg :: Term -> Term
neg p = implies p falsity
    
conj :: [Term] -> Term
conj [] = truth
conj (p:ps) = neg (implies p (neg (conj ps)))
    
  
reflexivity :: Fail.Can m => Term -> m Term
reflexivity (Leq x y) 
  | x == y = return truth
reflexivity _ = Fail.here


bottom :: Fail.Can m => Term -> m Term
bottom (Leq (Bot _) _) = return truth
bottom (Leq x (Bot _)) 
  | isCon (leftmost x) = return falsity
bottom _ = Fail.here


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


rewriteLeq :: Step m => Term -> m Term
rewriteLeq leq@(Leq x y) = 
  History.check Name.RewriteLeq leq $ do
    x' <- Transform.continue x  
    Fail.when (x' == x)
    Transform.continue (Leq x' y)
rewriteLeq _ = Fail.here
  

rewriteEq :: Step m => Term -> m Term
rewriteEq leq@(Leq x y) =
  History.check Name.RewriteEq leq $ do
    x' <- runEquiv x
    y' <- runEquiv y
    Fail.when (x' == x && y' == y)
    Transform.continue (Leq x' y')
rewriteEq _ = Fail.here


forAll :: Step m => Term -> m Term
forAll leq@(Leq (Lam b x) y) = 
  History.check Name.Forall leq
    . Transform.continue 
    $ Lam b (Leq x y')
  where 
  y' = app (Indices.lift y) [Var 0]
forAll _ = Fail.here


lfp :: forall m . Step m => Term -> m Term
lfp (Leq x y) = do
  Fail.unless (isFixPromoted x)
  History.check Name.LFP (Leq x y) $ do
    let x' = fixInduction x y
    leq' <- Transform.continue (Leq x' y)
    return leq'
  where
  -- > fixInduction ((fix F) x) y = F (\x -> y)
  fixInduction :: Term -> Term -> Term
  fixInduction (App (Fix _ _ fix_t) xs) y = id
    . (\f -> Term.reduce f xs)
    . Indices.subst (Term.abstractVars x_bs x_vars y)
    $ fix_t
    where
    x_vars = map fromVar xs
    x_bs = fst (flattenLam fix_t)
    
lfp _ = Fail.here


caseSplit :: Step m => Term -> m Term
caseSplit leq@(Leq (Case cse_t alts) y) = do
  Fail.when (isVar (leftmost y) 
    && not (Term.isSubterm cse_t y))
  History.check Name.CaseSplit leq $ do
    let leq' = Case cse_t (map leqAlt alts)
    Transform.continue leq'
  where
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq alt_t y')
    where
    y' = Indices.liftMany (nlength bs) y
    
caseSplit leq@(Leq left_t (Case cse_t@(Var x) alts)) = do
  Fail.unless (x `Indices.freeWithin` left_t)
  x_ty <- Type.getM cse_t
  let left_t_bot = Simp.run (Indices.substAt x (Bot x_ty) left_t)
  Fail.unless (isBot left_t_bot)
  History.check Name.CaseSplit leq $ do
    let leq' = Case cse_t (map leqAlt alts)
    Transform.continue leq'
  where
  leqAlt (Alt tc bs alt_t) =
    Alt tc bs (Leq left_t' alt_t)
    where
    left_t' = Indices.liftMany (nlength bs) left_t
    
caseSplit _ = Fail.here



{-
partial :: forall m . Step m => Term -> m Term
partial (Leq x y) = do
  x' <- Transform.continue x
  reduce x' y
  where
  
  reduce :: Term -> Term -> m Term
  reduce x y
    | x == y = 
      return Term.true
     
  reduce x@(Lam b x') y = id
    . Env.bind b
    . reduce x' 
    . Eval.run
    $ app (Indices.lift y) [Var 0]
   
  reduce x y@(Lam {}) =
    reduce y x
    
  reduce x@(flattenApp -> Con cx : xs) 
         y@(flattenApp -> Con cy : ys)
    | cx /= cy =
      return Term.false
      
    | otherwise = 
      History.check Name.EqReduceCon (Leq x y) $ do
        eqs <- zipWithM reduce xs ys
        Transform.continue (foldr Term.and Term.true eqs)
       
  reduce x@(flattenApp -> Con tcx : xs) y = 
    History.check Name.EqMatchCon (Leq x y) $ do
      alts <- mapM getAlt all_cons
      Transform.continue (Case y alts)
    where
    all_cons = id
      . Type.constructors 
      . get Type.constructorOf 
      $ Tag.untag tcx
    
    getAlt con
      | con /= Tag.untag tcx =   
        return (Alt tcon alt_bs Term.false)
        
      | otherwise = do
        let alt_t = Leq (Indices.liftMany (nlength alt_bs) x) pat_t 
        return (Alt tcon alt_bs alt_t)
      where
      tcon = Tag.with Tag.null con
      pat_t = (patternTerm . makePattern tcon) alt_bs
      alt_bs = Type.makeAltBindings con
 
  reduce x y
    | isCon (leftmost y)
    , not (isCon (leftmost x)) = reduce y x
        
  reduce (Case cse_t alts) y = id
    . Transform.continue 
    . Case cse_t
    $ map reduceAlt alts
    where
    reduceAlt (Alt con bs alt_t) = 
      Alt con bs (Leq alt_t y')
      where
      y' = Indices.liftMany (nlength bs) y
  
  reduce x y@(Case {}) =
    reduce y x
        
  reduce x y
    | isFixPromoted x
    , not (failedFixPromotion y) = 
      History.check Name.EqInduction (Leq x y) $ do
        x' <- fixInduction x y
        Type.assertEqM "[fix-induction]" x x'
        reduce x' y
      
    | isFixPromoted y
    , not (failedFixPromotion x) = 
      History.check Name.EqInduction (Leq y x) $ do
        y' <- fixInduction y x
        Type.assertEqM "[fix-induction]" y y'
        reduce x y'
    where
    -- There is really no point trying fixed-point induction if one
    -- side of the equation failed to be converted to fixed-point
    -- promoted form
    failedFixPromotion :: Term -> Bool
    failedFixPromotion t =
      not (isFixPromoted t) 
      && isFix (leftmost t)
      
    
    -- > fixInduction ((fix F) x) y = F (\x -> y)
    fixInduction :: Term -> Term -> m Term
    fixInduction (App (Fix _ _ fix_t) xs) y = id
      . Transform.continue 
      . (\f -> app f xs)
      . Indices.subst (Term.abstractVars x_bs x_vars y)
      $ fix_t
      where
      x_vars = map fromVar xs
      x_bs = fst (flattenLam fix_t)
      
  reduce x y = return (Leq x y)
    
    
equality _ = Fail.here
  -}
  
  
