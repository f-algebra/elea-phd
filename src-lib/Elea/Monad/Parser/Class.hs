module Elea.Monad.Parser.Class
(
  State (..),
  DefinedTerm (..),
)
where

import Elea.Prelude hiding ( State )
import Elea.Term
import Elea.Type
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Error.Class as Err

data DefinedTerm
  = DefinedName (Poly (Typed Name))
  | DefinedCon (Poly Constructor)

class Env.Bindings m => State m where
  defineName :: String -> Poly (Typed Name) -> m ()
  defineName l n = defineTerm l (DefinedName n)
  
  defineCon :: String -> Poly Constructor -> m ()
  defineCon l c = defineTerm l (DefinedCon c)
  
  defineTerm :: String -> DefinedTerm -> m ()
  defineInd :: String -> Poly Ind -> m ()
  
  lookupTerm :: String -> m (Maybe DefinedTerm)
  lookupInd :: String -> m (Maybe (Poly Ind))

