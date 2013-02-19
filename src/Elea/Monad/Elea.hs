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
import Elea.Term ( Term )
import Elea.Type ( Type, Bind )
import Elea.Show ( showM )
import qualified Elea.Type as Type
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Logging as Log
import qualified Elea.Monad.Error as Error
import qualified Elea.Monad.Failure as Fail
import qualified Data.Map as Map

data Defs 
  = Defs  { _terms :: Map String Term
          , _types :: Map String Type }

data EleaValue a
  = Fail
  | Error !Error.Type
  | Value !a
  
data EleaState a
  = ES  { defs :: !Defs
        , log :: ![String]
        , value :: !(EleaValue a) }
  
newtype Elea a 
  = Elea      { runElea :: (Defs, [Bind]) -> EleaState a }
  
mkLabels [''Defs]
 
unsafe :: Elea a -> a
unsafe (Elea f) = unsafePerformIO $ do
  mapM_ putStrLn log
  case val of
    Fail -> fail "FAIL"
    Error e -> fail ("ERROR: " ++ e)
    Value x -> return x
  where
  ES _ log val = f (Defs mempty mempty, mempty)
  
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
  Elea el >>= f = Elea $ \env ->
    let el_st = el env in 
    case value el_st of
      Fail -> el_st { value = Fail }
      Error e -> el_st { value = Error e }
      Value x -> 
        let el_st' = runElea (f x) (set fst (defs el_st) env) in
        el_st' { log = log (el_st) ++ log (el_st') } 
        
instance MonadReader [Bind] Elea where
  ask = Elea $ \(ds, bs) -> ES ds [] (Value bs)
  local f el = Elea $ \(ds, bs) -> runElea el (ds, f bs)
        
instance Type.Env Elea where
  bindAt at b = local (insertAt (convertEnum at) b)
  
instance Type.ReadableEnv Elea where
  boundAt at = do
    bs <- ask
    bs_s <- liftM show (mapM showM bs)
    return (debugNth (bs_s ++ " !! " ++ show at) bs (fromEnum at))
  
eleaLookup :: (Defs :-> Map String a) -> String -> Elea (Maybe a)
eleaLookup field name = Elea $ \(ds, _) -> 
  ES ds [] (Value (Map.lookup name (get field ds)))
  
eleaDefine :: (Defs :-> Map String a) -> String -> a -> Elea ()
eleaDefine field name el = Elea $ \(ds, _) ->
  if Map.member name (get field ds) 
  then ES ds [] (Error $ show name ++ " is already defined")
  else ES (modify field (Map.insert name el) ds) [] (Value ())
        
instance Defs.Monad Elea where
  lookupTerm = eleaLookup terms
  lookupType = eleaLookup types
  
  defineTerm name term = do
   -- term_s <- showM term
    eleaDefine terms name term
 --   Log.info $ "\nval " ++ name ++ " = " ++ term_s
    
  defineType name ty = do
    type_s <- showM ty
    eleaDefine types name ty
    Log.info $ "\ntype " ++ name ++ " = " ++ type_s
  
instance Error.Monad Elea where
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
  info = logMsg . ("Info: " ++)
  error = logMsg . ("Error: " ++)

logMsg :: String -> Elea ()
logMsg msg = Elea $ \(ds, _) -> ES ds [msg] (Value ())


