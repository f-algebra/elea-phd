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
  ( Env.Read m
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
    
  reduce (flattenApp -> Con cx : xs) 
         (flattenApp -> Con cy : ys)
    | cx /= cy =
      return Term.false
      
    | otherwise = do
        eqs <- zipWithM reduce xs ys
        Transform.continue (foldr Term.and Term.true eqs)
       
  reduce x@(flattenApp -> Con cx : xs) y = do
    alts <- mapM getAlt all_cons
    Transform.continue (Case y alts)
    where
    all_cons = Type.constructors (get Type.constructorOf cx)
    
    getAlt c 
      | c /= cx =   
        return (Alt c alt_bs Term.false)
        
      | otherwise = do
        alt_t <- reduce (altPattern c) x
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
    | isFixPromoted x = do
      x' <- fixInduction x y
      reduce x' y
      
    | isFixPromoted y = do
      y' <- fixInduction y x
      reduce x y'
    where
    isFixPromoted :: Term -> Bool
    isFixPromoted (App fix@(Fix {}) xs) =
      all isVar xs 
      && length x_vars == Set.size x_vars_set
      && Set.null overlap
      where
      x_vars = map fromVar xs
      x_vars_set = Set.fromList x_vars
      overlap = Set.intersection (Indices.free fix) x_vars_set
    isFixPromoted _ = False
    
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
      
    
  -- | Saves me writing out the fix cases again twice    
  reduce x y@(App (Fix {}) _) 
    | not (isFix (leftmost x)) =
      reduce y x

  reduce x y = 
    return (Eql x y)
    
    
equality _ = Fail.here
  
  
  
