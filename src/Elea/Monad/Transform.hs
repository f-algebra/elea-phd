-- | A transformation monad carries around a 'Term -> m Term' function
-- allowing it to apply the transformation recursively.
{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Transform
(
  Step (..),
  StepT (..),
  Env (..),
  compose,
  mapStepT,
  whenTraceSteps,
)
where

import Elea.Prelude hiding ( liftCatch )
import Elea.Term
import qualified Elea.Type as Type
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.History as History
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.StepCounter as Steps
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Reader as Reader hiding ( ask )
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Poset as Quasi
import qualified Data.Map as Map


{-# INLINE compose #-}

class (Steps.Limiter m, Fail.Can m) => Step m where
  -- | Accessing the recursive call to our transformation
  continue :: Term -> m Term

-- | Carry around a call to a simplification function
newtype StepT m a 
  = StepT { stepT :: ReaderT (Term -> m Term) (MaybeT m) a }
  deriving ( Functor, Applicative, Monad )
  
mapStepT :: forall m a b . Monad m 
  => (m (Maybe a) -> m (Maybe b)) -> StepT m a -> StepT m b
mapStepT f = StepT . mapReaderT (mapMaybeT f) . stepT
    
compose :: forall m . (Steps.Limiter m, Env.Read m, History.Env m, Defs.Read m, Env m) 
  => [(String, Term -> StepT m Term)] -> Term -> m Term
compose all_steps = applyOneStep all_steps
  where
  applyOneStep :: [(String, Term -> StepT m Term)] -> Term -> m Term
  applyOneStep [] term = return term
  applyOneStep ((step_name, step_fun) : steps) term = do
    mby_term' <- runMaybeT (runReaderT (stepT (step_fun term)) (compose all_steps))
    case mby_term' of
      Nothing -> applyOneStep steps term
      Just term' -> do
        Steps.take
        let valid_rewrite = id
              . Assert.augment (printf "within step \"%s\"" step_name)
              $ Term.assertValidRewrite term term'
        full_term' <- applyContext term'
        Assert.check valid_rewrite
          . whenTraceSteps (printf "Applied step \"%s\", yielding: %s" step_name full_term')
          $ return term'

liftCatch :: Monad m 
  => (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a))
  -> StepT m a -> (e -> StepT m a) -> StepT m a
liftCatch catch step_t handle = 
  StepT (catch' (stepT step_t) handle')
  where
  handle' = stepT . handle
  catch' = Reader.liftCatch (Maybe.liftCatch catch)

instance MonadTrans StepT where
  lift m = StepT (ReaderT (\_ -> Trans.lift m))
  
instance Steps.Limiter m => Step (StepT m) where
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

instance Err.Throws m => Err.Throws (StepT m) where
  type Err (StepT m) = Err.Err m
  throw = Trans.lift . Err.throw
  catch = liftCatch Err.catch
  augment e = mapStepT (Err.augment e)
  
instance Tag.Gen m => Tag.Gen (StepT m) where
  generateId = Trans.lift Tag.generateId
  
instance Fusion.Env m => Fusion.Env (StepT m) where
  rewrites = Trans.lift Fusion.rewrites
  local a t x = mapStepT (Fusion.local a t x)
  forgetRewrites = mapStepT Fusion.forgetRewrites
  disable = mapStepT Fusion.disable
  isDisabled = Trans.lift Fusion.isDisabled

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

instance Steps.Counter m => Steps.Counter (StepT m) where
  take = Trans.lift Steps.take
  listen = StepT . Steps.listen . stepT

instance Steps.Limiter m => Steps.Limiter (StepT m) where
  limit n = StepT . Steps.limit n . stepT
  remaining = Trans.lift Steps.remaining

instance Env m => Env (StepT m) where
  clearContext = mapStepT clearContext
  augmentContext = mapStepT . augmentContext
  applyContext = Trans.lift . applyContext
  traceSteps = Trans.lift traceSteps
  enableTraceSteps = mapStepT enableTraceSteps

class Monad m => Env m where
  applyContext :: Term -> m Term
  augmentContext :: (Term -> Term) -> m a -> m a
  clearContext :: m a -> m a

  traceSteps :: m Bool
  enableTraceSteps :: m a -> m a

{-# INLINE whenTraceSteps #-}
whenTraceSteps :: Env m => String -> m a -> m a
#ifndef TRACE
whenTraceSteps _ = id
#else
whenTraceSteps msg run = do
  should <- traceSteps
  if should
  then trace msg run
  else run
#endif
