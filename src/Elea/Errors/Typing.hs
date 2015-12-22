module Elea.Errors.Typing
(
  cannotTypeOmega,
  unboundIndex,
  invalidArguments,
  patternMatchNonInductive,
  patternsOutOfOrder,
  incorrectPattern,
  incorrectAltType,
  whileChecking,
  fixBodyBadlyTyped,
)
where

import Elea.Prelude
import qualified Elea.Monad.Error.Class as Err

typingErr :: Err.Throws m => String -> m a
typingErr = Err.augment "[Typing Error]\n" . Err.throw

whileChecking :: Err.Throws m => String -> m a -> m a
whileChecking t =
  Err.augment $ "Within term: " ++ t
  
cannotTypeOmega, 
  patternsOutOfOrder 
  :: Err.Throws m => m a
  
unboundIndex, 
  patternMatchNonInductive, 
  incorrectPattern
  :: Err.Throws m => String -> m a
  
invalidArguments,
  incorrectAltType
  :: Err.Throws m => String -> String -> m a
  
fixBodyBadlyTyped
  :: Err.Throws m => String -> String -> String -> m a

cannotTypeOmega =
  typingErr $ "Attempted to type the omega de-Bruijn index."
  
unboundIndex x = 
  typingErr $ "Cannot type an unbound de-Bruijn index [" ++ x ++ "]"
  
invalidArguments fun args = 
  typingErr 
    $ "Argument types do not match."
    ++ " Applying " ++ args ++ " to " ++ fun 
    
patternMatchNonInductive ty = 
  typingErr
    $ "Pattern matching on a non-inductive type [" ++ ty ++ "]"
    
patternsOutOfOrder =
  typingErr
    $ "Somehow the order of patterns " 
    ++ "doesn't match the order of constructors."
    
incorrectPattern con = 
  typingErr $ "Badly formed pattern: [" ++ show con ++ "]"
  
incorrectAltType exp got = 
  typingErr 
    $ "Return type of a pattern branch does not match the others."
    ++ "\nExpected: " ++ exp
    ++ "\nGot: " ++ got
    
fixBodyBadlyTyped body_s b_ty body_ty = 
  typingErr
    $ "Type of fixpoint body doesn't match that of fixpoint binding."
    ++ "\nBody: " ++ body_s
    ++ "\nHas type: " ++ body_ty
    ++ "\nWhich doesn't match binding type: " ++ b_ty
