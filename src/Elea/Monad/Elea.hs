-- | This module defines the 'Elea' 'Monad', which handles errors, 
-- failing computations, logging, and storing/looking-up 
-- definitions statefully.
module Elea.Monad.Elea
(
  Elea, run
)
where

import Prelude ()
import Elea.Prelude hiding ( log )
import Elea.Index
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Foldable as Fold
import qualified Elea.Env as Env
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State
import qualified Data.Set as Set

type Defs = Map String Term

data EleaValue a
  = Fail
  | Error !Err.Err
  | Value !a
  deriving ( Functor )
        
data EleaRead
  = ER  { _readBinds :: ![Bind] }
  
newtype Elea a 
  = Elea { runElea :: EleaRead -> EleaValue a }
 
mkLabels [''EleaRead]

run :: Elea a -> a
run el =
  case runElea el (ER mempty) of
    Value x -> x
    Fail -> error "FAIL"
    Error e -> error e

instance Functor Elea where
  fmap f (Elea g) = Elea (fmap f . g)
  
instance Monad Elea where
  return x = Elea $ \_ -> (Value x)
  Elea el >>= f = Elea $ \r ->
    case el r of
      Fail -> Fail
      Error e -> Error e
      Value x -> runElea (f x) r
        
instance MonadReader EleaRead Elea where
  ask = Elea Value
  local f el = Elea (runElea el . f)

instance Env.Writable Elea where
  bindAt at b = 
    local (modify readBinds (insertAt (enum at) b))
    
  matched _ _ = id
    
instance Env.Readable Elea where
  boundAt at = do
    bs <- Env.bindings
    Err.when (enum at >= length bs)
      $ "Index " ++ show at ++ " not bound in " ++ show bs
    return (bs !! fromEnum at)

instance Err.Monad Elea where
  throw e = Elea $ \_ -> (Error e)
  catch (Elea el) handle = Elea $ \r ->
    case el r of
      Error e -> runElea (handle e) r
      other -> other 

instance Fail.Monad Elea where
  here = Elea $ \_ -> Fail

