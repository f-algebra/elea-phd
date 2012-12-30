module Elea.Equation
(
  Equation (..), 
  mapTerms,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..) )

import qualified Elea.Term as Term

data Equation 
  = Equals Term Term

instance Biplate Equation Term where
  biplate (Equals t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> Equals t1 t2)

mapTerms :: (Term -> Term) -> Equation -> Equation
mapTerms = mapChildrenBi

