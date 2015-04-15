-- | A transformation monad carries around a 'Term -> m Term' function
-- allowing it to apply the transformation recursively.
module Elea.Monad.Transform
(
  Step (..),
  StepT (..),
  fix,
  compose,
  traceCont,
  mapStepT,
)
where

import Elea.Prelude
import Elea.Term 
import Elea.Show ()
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.History as History
import qualified Elea.Embed as Embed
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Height as Height
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Poset as Partial


-- | Accessing the recursive call to our transformation 
class Monad m => Step m where
  continue :: Term -> m Term
  
-- | Carry around a call to a simplification function
newtype StepT m a 
  = StepT { stepT :: ReaderT (Term -> m Term) (MaybeT m) a }
  deriving ( Functor, Monad )
  
mapStepT :: forall m a b . Monad m 
  => (m (Maybe a) -> m (Maybe b)) -> StepT m a -> StepT m b
mapStepT f = StepT . mapReaderT (mapMaybeT f) . stepT
    

fix :: forall m . History.Env m 
  => (Term -> StepT m Term) -> Term -> m Term
fix f t = do
  hist <- History.ask
  mby_t' <- runMaybeT (runReaderT (stepT (f t)) (run hist))
  case mby_t' of
    Nothing -> return t
    Just t' -> do
      if not (Type.has t)
        || not (Type.has t')
        || Type.get t == Type.get t'
      then return t'
      else error
          $ "[type error] transforming:\n"
          ++ show t ++ " : " ++ show (Type.get t)
          ++ "\ninto:\n" 
          ++ show t' ++ " : " ++ show (Type.get t') 
          ++ "\n"
  where
  -- Our recursive transformation, a wrapper around 'f'
  run :: History.Repr -> Term -> m Term
  run hist t' = do
    -- Check term size has shrunk
    if t' Partial.< t
    then fix f t'
    else do
      hist' <- History.ask
      if nlength hist' > nlength hist 
      then fix f t'
      -- ^ We can only shrink the term in the recursive call
      -- if we add a term to the history. Otherwise we
      -- have an increasing recursive call and should throw an error.
      else error
        $ "\n\n[error] Recursive call triggered by [" 
        ++ show (Height.get t) ++ "]\n" ++ show t
        ++ "\n\nGiven a larger term [" ++ show (Height.get t') 
        ++ "]:\n" ++ show t' ++ "\n"
    

traceCont :: Step m => String -> Term -> Term -> m Term
traceCont step_name orig new = 
  trace 
    ("\n\n[" ++ step_name ++ "]\n" ++ show orig ++ "\n==>\n" ++ show new) 
    (continue new)

compose :: Fail.Can m => [Term -> m Term] -> Term -> m Term
compose [] _ = Fail.here
compose (f:fs) t = do
  mby_t' <- Fail.catch (f t)
  case mby_t' of
    Nothing -> compose fs t
    Just t' -> return t'

    
instance MonadTrans StepT where
  lift m = StepT (ReaderT (\_ -> Trans.lift m))
  
instance Monad m => Step (StepT m) where
  continue t = do
    f <- StepT Reader.ask
    Trans.lift (f t)
    
instance Monad m => Fail.Can (StepT m) where
  here = StepT Fail.here
  catch = StepT . Fail.catch . stepT
    
instance Step m => Step (MaybeT m) where
  continue = Trans.lift . continue
  
instance Env.Read m => Env.Read (StepT m) where
  bindings = Trans.lift Env.bindings
  
instance Env.Write m => Env.Write (StepT m) where
  bindAt at b = mapStepT (Env.bindAt at b)
  matched t c = mapStepT (Env.matched t c)

instance Defs.Read m => Defs.Read (StepT m) where
  lookupTerm n = Trans.lift . Defs.lookupTerm n
  lookupType n = Trans.lift . Defs.lookupType n
  lookupName = Trans.lift . Defs.lookupName
  
instance Tag.Gen m => Tag.Gen (StepT m) where
  generateId = Trans.lift Tag.generateId
  
instance Rewrite.Env m => Rewrite.Env (StepT m) where
  rewrites = Trans.lift Rewrite.rewrites
  local a t x = mapStepT (Rewrite.local a t x)

instance Env.MatchRead m => Env.MatchRead (StepT m) where
  matches = Trans.lift Env.matches
  
instance Discovery.Tells m => Discovery.Tells (StepT m) where
  tell = Trans.lift . Discovery.tell
  
instance History.Env m => History.Env (StepT m) where
  ask = Trans.lift History.ask
  seeCode x = mapStepT (History.seeCode x)
  

