module Elea.Discoveries
(
  EquationSet, 
  singleton,
  toList,
  fromList,
)
where

import Prelude ()
import Elea.Prelude hiding ( toList )
import Elea.Term

-- | A set of equations which have been discovered.
-- Its 'Monoid' instance is important, as this decides how equations subsume
-- or combine with each other. 
-- Right now I've given it a temporary trivial definition.
newtype EquationSet
  = EqSet { runEqSet :: [Equation] }
  
singleton :: Equation -> EquationSet
singleton eq = EqSet [eq]

toList :: EquationSet -> [Equation]
toList = runEqSet                  

fromList :: [Equation] -> EquationSet
fromList = EqSet
  
instance Monoid EquationSet where
  mempty = EqSet []
  mappend (EqSet es1) (EqSet es2) = 
    EqSet (es1 ++ es2)
   
