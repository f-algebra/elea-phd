module Elea.Errors.Parsing
(
  termNotFound,
  literalNotSupported,
  typeNotFound,
  invalidConstructor,
  invalidTypeArgs,
  nonInductiveMatch,
)
where

import Elea.Prelude
import qualified Elea.Monad.Error.Class as Err

parsingErr :: Err.Throws m => String -> m a
parsingErr = Err.augment "[Parsing Error]\n" . Err.throw
  
brkt x = " [" ++ x ++ "] "

termNotFound x = 
  parsingErr $ "Undefined term" ++ brkt x
  
typeNotFound x = 
  parsingErr $ "Undefined inductive type" ++ brkt x
  
literalNotSupported x =
  parsingErr $ "Literal not supported" ++ brkt x
  
invalidConstructor ind con = 
  parsingErr 
    $ "The name" ++ brkt con 
    ++ "is not a constructor of type" ++ brkt ind
  
invalidTypeArgs x tys = 
  parsingErr 
    $ "The type arguments" ++ brkt (intercalate ", " tys)
    ++ "are invalid for the name" ++ brkt x
    
nonInductiveMatch non_ind = 
  parsingErr
    $ "Cannot pattern match over a non-inductive type" ++ brkt non_ind
