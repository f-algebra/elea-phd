-- | This module defines the 'Elea' 'Monad', which handles errors, 
-- failing computations, logging, and storing/looking-up 
-- definitions statefully.
module Elea.Monad.Elea
(
  Elea, unsafe
)
where

import Prelude ()
import Elea.Prelude hiding ( log )
import Elea.Index
import Elea.Term ( Term, Bind )
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Env as Env
import qualified Elea.Typing as Typing
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Logging as Log
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State
import qualified Data.Map as Map

type Defs = Map String Term

data EleaValue a
  = Fail
  | Error !Err.Err
  | Value !a
  
data EleaState a
  = ES  { defs :: !Defs
        , log :: ![String]
        , value :: !(EleaValue a) }
  
newtype Elea a 
  = Elea      { runElea :: (Defs, [Bind]) -> EleaState a }
 
unsafe :: Elea a -> a
unsafe (Elea f) = unsafePerformIO $ do
  -- mapM_ putStrLn log
  case val of
    Fail -> fail "FAIL"
    Error e -> fail ("ERROR: " ++ e)
    Value x -> return x
  where
  ES _ log val = f (mempty, mempty)
  
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
  return x = Elea $ \(ds, _) -> ES ds [] (Value x)
  Elea el >>= f = Elea $ \(ds, bs) ->
    let el_st = el (ds, bs) in 
    case value el_st of
      Fail -> el_st { value = Fail }
      Error e -> el_st { value = Error e }
      Value x -> 
        let el_st' = runElea (f x) (defs el_st, bs) in
        el_st' { log = log (el_st) ++ log (el_st') } 
        
instance MonadReader [Bind] Elea where
  ask = Elea $ \(ds, bs) -> ES ds [] (Value bs)
  local f el = Elea $ \(ds, bs) -> runElea el (ds, f bs)
  
instance MonadState Defs Elea where
  get = Elea $ \(ds, _) -> ES ds [] (Value ds)
  put ds = Elea $ \_ -> ES ds [] (Value ())

instance Env.Writable Elea where
  bindAt at b = 
      local 
    $ insertAt (convertEnum at) (liftAt at b) 
    . map (liftAt at)
  
  equals _ _ = id
    
instance Env.Readable Elea where
  boundAt at = do
    bs <- ask
    Err.when (fromEnum at >= length bs)
      $ "Index " ++ show at ++ " not bound in " ++ show bs
    return (bs !! fromEnum at)
    
  bindingDepth = 
    asks (toEnum . pred . length)

instance Defs.Monad Elea where
  lookup name = State.gets (Map.lookup name)
  
  add name term = do
    term_s <- showM term
    term_ty <- Typing.typeOf term
    ty_s <- showM term_ty
    Log.info
      $  "\nval " ++ name ++ ": " ++ ty_s ++ " = " ++ term_s
    State.modify (Map.insert name term)
  
instance Err.Monad Elea where
  throw e = Elea $ \(ds, _) ->
    ES ds [] (Error e)
  catch (Elea el) handle = Elea $ \env ->
    let el_st = el env in
    case value el_st of
      Error e -> runElea (handle e) env
      _ -> el_st

instance Fail.Monad Elea where
  here = Elea $ \(ds, _) -> ES ds [] Fail

instance Log.Monad Elea where
  info msg = Elea $ \(ds, _) -> ES ds [msg] (Value ())

