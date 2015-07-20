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
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Term.Tag as Tag
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Poset as Quasi
import qualified Data.Map as Map

{-# INLINEABLE fix #-}
{-# INLINEABLE compose #-}


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
    

fix :: forall m . (Env.Read m, History.Env m) 
  => (Term -> StepT m Term) -> Term -> m Term
fix f t = do
  hist <- History.ask
  mby_t' <- runMaybeT (runReaderT (stepT (f t)) (fix f))
  case mby_t' of
    Just t' -> do
      Type.assertEqM "[fix]" t t' 
      return t'
    _ -> 
      return t
  

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
  matched m = mapStepT (Env.matched m)
  forgetMatches w = mapStepT (Env.forgetMatches w)

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
  local f = mapStepT (History.local f)
  
instance Direction.Has m => Direction.Has (StepT m) where
  get = Trans.lift Direction.get
  local d = mapStepT (Direction.local d)
  
instance Memo.Can m => Memo.Can (StepT m) where
  maybeMemo n t = mapStepT instep
    where
    instep :: m (Maybe (Maybe Term)) -> m (Maybe (Maybe Term))
    instep mx =
      liftM return (Memo.maybeMemo n t (liftM join mx))
        
        
        
      
