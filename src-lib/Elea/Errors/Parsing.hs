module Elea.Error.Parsing
(
  variableNotFound,
  literalNotSupported,
  typeArgsNotSupported
)
where

import Elea.Prelude
import qualified Elea.Monad.Error.Class as Err

parsingErr :: Err.Can m => String -> m a
parsingErr = Err.augment "[Parsing Error]\n" . Err.throw
  
variableNotFound x = 
  parsingErr $ "Variable not found: " ++ x
  
literalNotSupported x =
  parsingErr $ "Literal not supported: " ++ x
  
typeArgsNotSupported = 
  parsingErr "Type arguments are not supported. Simple types only."
  
mutualRecursionNotSupported x =
  parsingErr $ "Mutual recursion not supported: " ++ s
  
