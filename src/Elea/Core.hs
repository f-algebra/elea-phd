-- | The 'Elea' 'Monad', which handles errors, failing computations, 
-- logging, and storing/looking-up definitions statefully.
module Elea.Core
(
  Elea, Notes, unsafe
)
where

import Prelude ()
import Elea.Prelude hiding ( log )
import Elea.Term ( Term )

import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Logging as Log
import qualified Elea.Monad.Error as Error
import qualified Elea.Monad.Failure as Fail
import qualified Data.Map as Map

type Notes = ()
type Defs = Map String (Term Notes)

data EleaValue a
  = Fail
  | Error !Error.Type
  | Value !a
  
data EleaState a
  = ES  { defs :: !Defs
        , log :: ![String]
        , value :: !(EleaValue a) }
  
newtype Elea a 
  = Elea      { runElea :: Defs -> EleaState a }
  
unsafe :: Elea a -> a
unsafe (Elea f) = unsafePerformIO $ do
  mapM_ putStrLn log
  case val of
    Fail -> fail "FAIL"
    Error e -> fail ("ERROR: " ++ e)
    Value x -> return x
  where
  ES _ log val = f mempty
  
instance Functor EleaValue where
  fmap f (Value x) = Value (f x)
  fmap f Fail = Fail
  fmap f (Error e) = Error e
  
instance Functor EleaState where
  fmap f el_st = 
    el_st { value = fmap f (value el_st) }
  
instance Functor Elea where
  fmap f (Elea g) = Elea (fmap f . g)
  
instance Monad Elea where
  return x = Elea $ \ds -> ES ds [] (Value x)
  Elea el >>= f = Elea $ \ds ->
    let el_st = el ds in 
    case value el_st of
      Fail -> el_st { value = Fail }
      Error e -> el_st { value = Error e }
      Value x -> 
        let el_st' = runElea (f x) (defs el_st) in
        el_st' { log = log (el_st) ++ log (el_st') } 
        
instance Defs.Monad Notes Elea where
  lookup n = Elea $ \ds -> 
    ES ds [] (Value (Map.lookup n ds))
  add n t = Elea $ \ds ->
    if Map.member n ds 
    then ES ds [] (Error $ show n ++ " is already defined")
    else ES (Map.insert n t ds) [] (Value ())
      
instance Error.Monad Elea where
  throw e = Elea $ \ds ->
    ES ds [] (Error e)
  catch (Elea el) handle = Elea $ \ds ->
    let el_st = el ds in
    case value el_st of
      Error e -> runElea (handle e) ds
      _ -> el_st

instance Fail.Monad Elea where
  here = Elea $ \ds -> ES ds [] Fail

instance Log.Monad Elea where
  info = logMsg . ("Info: " ++)
  error = logMsg . ("Error: " ++)

logMsg :: String -> Elea ()
logMsg msg = Elea $ \ds -> ES ds [msg] (Value ())
