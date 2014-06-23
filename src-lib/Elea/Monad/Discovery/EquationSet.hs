module Elea.Monad.Discovery.EquationSet
(
  EqSet, 
  singleton,
  toList,
)
where

import Elea.Prelude hiding ( toList )
import Elea.Term
import Elea.Monad.Env ()
import qualified Elea.Unification.Map as UMap

-- | A set of equations which have been discovered.
newtype EqSet
  = EqSet { runEqSet :: [Equation] }
  
singleton :: Equation -> EqSet
singleton eq = EqSet [eq]

toList :: EqSet -> [Equation]
toList = collapse . runEqSet
  where
  collapse :: [Equation] -> [Equation]
  collapse eqs = id
    . UMap.elems
    . foldr (uncurry UMap.insert) UMap.empty  
    $ map equationLHS eqs `zip` eqs
  
instance Monoid EqSet where
  mempty = EqSet mempty
  mappend (EqSet es1) (EqSet es2) = 
    EqSet (es1 ++ es2)

