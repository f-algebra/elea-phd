module Elea.Monad.Parser.Class
(
  State (..)
)
where

import Elea.Prelude hiding ( State )
import Elea.Term
import Elea.Type
import qualified Elea.Monad.Env.Class as Env

class Env.Bindings m => State m where
  defineTerm :: String -> Polymorphic Term -> m ()
  defineInd :: String -> Polymorphic Ind -> m ()
  
  lookupTerm :: String -> MaybeT m (Polymorphic Term)
  lookupInd :: String -> MaybeT m (Polymorphic Ind)
  

