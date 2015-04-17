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

type Step m = Eval.Step m


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
    | stripTags x == stripTags y = 
      return Term.true
      
  reduce (App (Con cx) xs) (App (Con cy) ys)
    | cx /= cy = return Term.false
    | otherwise = do
        eqs <- zipWithM reduce xs ys
        Transform.continue (foldr Term.and Term.true eqs)
        
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
        
  reduce x@(App fix@(Fix _ _ fix_t) xs) y
    | all isVar xs
    , length x_vars == Set.size x_vars_set
    , Set.null overlap = do
      x' <- id
        . Transform.continue 
        . (\f -> app f xs)
        . Indices.subst (Term.abstractVars x_bs x_vars y)
        $ fix_t
      reduce x' y
    where
    x_vars = map fromVar xs
    x_vars_set = Set.fromList x_vars
    x_bs = fst (flattenLam fix_t)
    overlap = Set.intersection (Indices.free fix) x_vars_set
    
  reduce x@(App fix@(Fix {}) xs) y 
    | any (isCon . leftmost) xs =
      History.check "eq-unfold" (Eql x y) $ do
        x' <- id
          . Transform.continue 
          $ app (Term.unfoldFix fix) xs 
        reduce x' y
        
  -- | Saves me writing out the fix cases again twice    
  reduce x y@(App (Fix {}) _) 
    | not (isFix (leftmost x)) =
      reduce y x

  reduce x y = 
    return (Eql x y)
    
    
equality _ = Fail.here
  
  
  
