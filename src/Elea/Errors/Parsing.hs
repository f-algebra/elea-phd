module Elea.Error.Parsing
(
  variableNotFound,
  literalNotSupported,
  typeArgsNotSupported
)
where

import Prelude ()
import Elea.Prelude
import qualified Elea.Monad.Error.Class as Err

data Error
  = VariableNotFound String
  | LiteralNotSupported String
  | TypeArgsNotSupported
  | MutualRecursionNotSupported String
  
variableNotFound = 
  Err.throw . VariableNotFound
literalNotSupported = 
  Err.throw . LiteralNotSupported
typeArgsNotSupported = 
  Err.throw TypeArgsNotSupported
mutualRecursionNotSupported =
  Err.throw . MutualRecursionNotSupported
  
instance Show Error where
  show e = "[Parsing Error] " ++ err e
    where
    err (VariableNotFound var) = 
      "Variable not found: " ++ var
 
    err (LiteralNotSupported lit) = 
      "Literal not supported: " ++ lit
      
    err TypeArgsNotSupported =
      "Type arguments not supported"
      
    err (MutualRecursionNotSupported s) = 
      "Mutual recursion not supported: " ++ s
