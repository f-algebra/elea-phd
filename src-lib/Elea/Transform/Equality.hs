module Elea.Transform.Equality
(
  steps
)
where


import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import Elea.Unification ( Unifier )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Height as Height
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Transform.Names as Name
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Monad.Failure.Class as Fail

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Poset as Partial

type Step m = 
  ( Env.All m
  , Eval.Step m )


steps :: Step m => [Term -> m Term]
steps = 
  [ const Fail.here   
  , equality
  ]
  
 
equality :: forall m . Step m => Term -> m Term
equality (Eql x y) = do
  x' <- Transform.continue x
  y' <- Transform.continue y
  reduce x' y'
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
      History.check Name.EqReduceCon (Eql x y) $ do
        eqs <- zipWithM reduce xs ys
        Transform.continue (foldr Term.and Term.true eqs)
       
  reduce x@(flattenApp -> Con cx : xs) y = 
    History.check Name.EqMatchCon (Eql x y) $ do
      alts <- mapM getAlt all_cons
      Transform.continue (Case y alts)
    where
    all_cons = Type.constructors (get Type.constructorOf cx)
    
    getAlt c 
      | c /= cx =   
        return (Alt c alt_bs Term.false)
        
      | otherwise = do
        let alt_t = Eql (altPattern c) (Indices.liftMany (length alt_bs) x)
        return (Alt c alt_bs alt_t)
      where
      alt_bs = Type.makeAltBindings c
 
  reduce x y
    | isCon (leftmost y)
    , not (isCon (leftmost x)) = reduce y x
        
  reduce (Case cse_t alts) y = id
    . Transform.continue 
    . Case cse_t
    $ map reduceAlt alts
    where
    reduceAlt (Alt con bs alt_t) = 
      Alt con bs (Eql alt_t y')
      where
      y' = Indices.liftMany (length bs) y
  
  reduce x y@(Case {}) =
    reduce y x
        
  reduce x y
    | isFixPromoted x
    , not (failedFixPromotion y) = 
      History.check Name.EqInduction (Eql x y) $ do
        x' <- fixInduction x y
        Type.assertEqM "[fix-induction]" x x'
        reduce x' y
      
    | isFixPromoted y
    , not (failedFixPromotion x) = 
      History.check Name.EqInduction (Eql y x) $ do
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
      
  reduce x y = return (Eql x y)
    
    
equality _ = Fail.here
  
  
  
