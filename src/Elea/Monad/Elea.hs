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
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Env as Env
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
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
  deriving ( Functor )
  
data EleaWrite a
  = EW  { _writeDefs :: !Defs
        , _writeLog :: ![String]
        , _writeValue :: !(EleaValue a) }
  deriving ( Functor )
        
data EleaRead
  = ER  { _readDefs :: !Defs
        , _readBinds :: ![Bind]
        , _readMatches :: !Matches }
  
newtype Elea a 
  = Elea      { runElea :: EleaRead -> EleaWrite a }
 
mkLabels [''EleaWrite, ''EleaRead]
  
unsafe :: Elea a -> a
unsafe (Elea el) = unsafePerformIO $ do
  -- mapM_ putStrLn log
  case val of
    Fail -> fail "FAIL"
    Error e -> fail ("ERROR: " ++ e)
    Value x -> return x
  where
  EW _ log val = el (ER mempty mempty mempty)
  
instance Functor Elea where
  fmap f (Elea g) = Elea (fmap f . g)
  
instance Monad Elea where
  return x = Elea $ \r -> EW (get readDefs r) [] (Value x)
  Elea el >>= f = Elea $ \r ->
    let EW defs log val = el r in 
    case val of
      Fail -> EW defs log Fail 
      Error e -> EW defs log (Error e)
      Value x -> 
        let EW defs' log' val' = runElea (f x) (set readDefs defs r) in
        EW defs' (log ++ log') val'
        
instance MonadReader ([Bind], Matches) Elea where
  ask = Elea $ \(ER ds bs mts) -> EW ds [] (Value (bs, mts))
  local f el = Elea $ \(ER ds bs mts) ->
    let (bs', mts') = f (bs, mts) in
    runElea el (ER ds bs' mts')
  
instance MonadState Defs Elea where
  get = Elea $ \(ER ds _ _) -> EW ds [] (Value ds)
  put ds = Elea $ \_ -> EW ds [] (Value ())

instance Env.Writable Elea where
  bindAt at b = local (mapBinds *** mapMatches)
    where
    mapBinds = insertAt (convertEnum at) (liftAt at b) . map (liftAt at)
    mapMatches = Map.mapKeysMonotonic (liftAt at) . fmap (liftAt at)
  
  equals t k =
    local (second (Map.insert t k))
    
  forgetMatches = 
    local (second (Map.filterWithKey (\k _ -> isVar k)))
    
instance Env.Readable Elea where
  bindings = asks fst
  matches = asks snd

  boundAt at = do
    bs <- Env.bindings
    Err.when (fromEnum at >= length bs)
      $ "Index " ++ show at ++ " not bound in " ++ show bs
    return (bs !! fromEnum at)

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
  throw e = Elea $ \r ->
    EW (get readDefs r) [] (Error e)
  catch (Elea el) handle = Elea $ \r ->
    let w = el r in
    case get writeValue w of
      Error e -> runElea (handle e) r
      _ -> w

instance Fail.Monad Elea where
  here = Elea $ \r -> EW (get readDefs r) [] Fail

instance Log.Monad Elea where
  info msg = Elea $ \r -> EW (get readDefs r) [msg] (Value ())

