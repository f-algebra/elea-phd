module Elea.Monad.Discovery.EquationSet
(
  EqSet (..), 
  singleton,
  toList,
  empty,
)
where

import Prelude ()
import Elea.Prelude hiding ( toList )
import Elea.Term
import Elea.Monad.Env ()
import Elea.Show ()
import qualified Elea.Unification.Map as UMap

-- | A set of rewrites which have been discovered.
newtype EqSet
  = EqSet { runEqSet :: [Prop] }
  
singleton :: Prop -> EqSet
singleton eq = EqSet [eq]

toList :: EqSet -> [Prop]
toList = collapse . runEqSet
  where
  collapse :: [Prop] -> [Prop]
  collapse = id
  {-
  collapse eqs = id
    . UMap.elems
    . foldr (uncurry UMap.insert) UMap.empty  
    $ map (get equationTerm) eqs `zip` eqs
    -}
    
empty :: EqSet -> Bool
empty = null . runEqSet
  
instance Monoid EqSet where
  mempty = EqSet mempty
  mappend (EqSet es1) (EqSet es2) = 
    EqSet (es1 ++ es2)

instance Show EqSet where
  show (EqSet []) = ""
  show (EqSet (p:ps)) = show p ++ "\n" ++ show (EqSet ps)