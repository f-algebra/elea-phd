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
import qualified Elea.Term as Term
import qualified Elea.Env as Env
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Floating as Float
import qualified Elea.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-# SPECIALISE 
  Fusion.run :: Term -> Elea Term #-} 
{-# SPECIALISE 
  Float.run :: Term -> Elea Term #-}

type Defs = Map String Term

data EleaValue a
  = Fail
  | Error !Err.Err
  | Value !a
  deriving ( Functor )
        
data EleaRead
  = ER  { _readBinds :: ![Bind]
        , _readMatches :: !Matches }
  
newtype Elea a 
  = Elea { runElea :: EleaRead -> EleaValue a }
 
mkLabels [''EleaRead]

run :: Elea a -> a
run el =
  case runElea el (ER mempty mempty) of
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
  bindAt at b = local mapAll
    where
    mapAll = id
      . modify readMatches mapMatches
      . modify readBinds mapBinds
      
    mapBinds = insertAt (convertEnum at) (liftAt at b) . map (liftAt at)
    mapMatches = Map.mapKeysMonotonic (liftAt at) . fmap (liftAt at *** succ)
    
  equals t k = 
    local (modify readMatches mapMatches)
    where
    mapMatches = Map.insert t (k, 0)
    
  filterMatches p = 
    local (modify readMatches (Map.filterWithKey (\k _ -> p k)))
    
instance Env.Readable Elea where
  bindings = asks (get readBinds)
  matches = asks (get readMatches)
  
  boundAt at = do
    bs <- Env.bindings
    Err.when (fromEnum at >= length bs)
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

