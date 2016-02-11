-- | A transformation monad carries around a 'Term -> m Term' function
-- allowing it to apply the transformation recursively.
{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Transform
(
  Step, 
  restart, traverse,
  Env (..),
  NamedStep, 
  step,
  silentStep,
  compose,
  mapRewriteT,
)
where

import Elea.Prelude hiding ( liftCatch, traverse )
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
import qualified Elea.Monad.Transform.TraceSteps as TraceSteps
import qualified Elea.Monad.Transform.Signals as Signals
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.StepCounter as Steps
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Reader as Reader hiding ( ask )
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Poset as Quasi
import qualified Data.Map as Map


class (Env m, Fail.Can m) => Step m where
  apply :: Term -> m Term

traverse :: Step m => (Term -> Term) -> Term -> m Term
traverse ctx = augmentContext ctx . apply

restart :: Step m => Term -> m Term
restart term = do
  step_name <- askStepName
  Signals.tellUsedAntecentRewrite
  TraceSteps.traceM (printf "\n<< within \"%s\" >>\n\n%s" step_name term)
  clearContext (apply term)

-- | Carry around a call to a simplification function
newtype RewriteT m a 
  = RewriteT { rewriteT :: ReaderT (Term -> m Term) (MaybeT m) a }
  deriving ( Functor, Applicative, Monad )
  
data NamedStep m = NamedStep 
  { rewriteStepName :: !String
  , rewriteStepSilent :: !Bool
  , rewriteStep :: Term -> RewriteT m Term }

step :: Monad m => String -> (Term -> RewriteT m Term) -> NamedStep m
step name rewrite = NamedStep 
  { rewriteStepName = name
  , rewriteStepSilent = False
  , rewriteStep = rewrite }

silentStep :: String -> (Term -> RewriteT m Term) -> NamedStep m
silentStep name rewrite = NamedStep
  { rewriteStepName = name
  , rewriteStepSilent = True
  , rewriteStep = rewrite }

mapRewriteT :: forall m a b . Monad m 
  => (m (Maybe a) -> m (Maybe b)) -> RewriteT m a -> RewriteT m b
mapRewriteT f = RewriteT . mapReaderT (mapMaybeT f) . rewriteT
    

{-# INLINE compose #-}
compose :: forall m . Env m 
  => [NamedStep m] -> Term -> m Term
compose all_steps = apply
  where
  apply :: Term -> m Term
  apply = applyOneStep all_steps

  applyOneStep :: [NamedStep m] -> Term -> m Term
  applyOneStep [] term = return term
  applyOneStep (named_step : steps) term = do
    (mby_term', signals) <- id
      . Signals.consume
      . localStepName step_name
      . runMaybeT 
      $ runReaderT (rewriteT (rewriteStep named_step term)) apply
    case mby_term' of
      Nothing -> do
        when (get Signals.usedAntecedentRewrite signals) $ do
          TraceSteps.traceM (printf "\n<< failed \"%s\" on >>\n\n%s" step_name term)
        applyOneStep steps term

      Just term' -> do
        full_term <- applyContext term
        full_term' <- applyContext term'
        Assert.checkM
          . Assert.augment (printf "within step \"%s\"" step_name)
          $ Term.assertValidRewrite full_term full_term'
        when (get Signals.usedAntecedentRewrite signals) $ do
          TraceSteps.traceM (printf "\n<< success \"%s\" on >>\n\n%s" step_name term)
        unless (rewriteStepSilent named_step) $ do
          TraceSteps.traceM (printf "\n< applying \"%s\" yielded >\n\n%s" step_name full_term')
        if get Signals.stopRewriting signals 
        then return term' 
        else apply term'   -- fingers crossed for tail-call optimisation
    where
    step_name = rewriteStepName named_step

liftCatch :: Monad m 
  => (m (Maybe a) -> (e -> m (Maybe a)) -> m (Maybe a))
  -> RewriteT m a -> (e -> RewriteT m a) -> RewriteT m a
liftCatch catch step_t handle = 
  RewriteT (catch' (rewriteT step_t) handle')
  where
  handle' = rewriteT . handle
  catch' = Reader.liftCatch (Maybe.liftCatch catch)

instance MonadTrans RewriteT where
  lift m = RewriteT (ReaderT (\_ -> Trans.lift m))
  
instance Env m => Step (RewriteT m) where
  apply term = do
    rewrite <- RewriteT Reader.ask
    Trans.lift (rewrite term)

instance Monad m => Fail.Can (RewriteT m) where
  here = RewriteT Fail.here
  catch = RewriteT . Fail.catch . rewriteT
    
instance (Env (MaybeT m), Step m) => Step (MaybeT m) where
  apply = Trans.lift . apply
  
instance Env.Read m => Env.Read (RewriteT m) where
  bindings = Trans.lift Env.bindings
  
instance Env.Write m => Env.Write (RewriteT m) where
  bindAt at b = mapRewriteT (Env.bindAt at b)
  matched m = mapRewriteT (Env.matched m)
  forgetMatches w = mapRewriteT (Env.forgetMatches w)

instance Defs.Read m => Defs.Read (RewriteT m) where
  lookupTerm n = Trans.lift . Defs.lookupTerm n
  lookupType n = Trans.lift . Defs.lookupType n
  lookupName = Trans.lift . Defs.lookupName

instance Err.Throws m => Err.Throws (RewriteT m) where
  type Err (RewriteT m) = Err.Err m
  throw = Trans.lift . Err.throw
  catch = liftCatch Err.catch
  augment e = mapRewriteT (Err.augment e)
  
instance Tag.Gen m => Tag.Gen (RewriteT m) where
  generateId = Trans.lift Tag.generateId
  
instance Fusion.Env m => Fusion.Env (RewriteT m) where
  rewrites = Trans.lift Fusion.rewrites
  local a t x = mapRewriteT (Fusion.local a t x)
  forgetRewrites = mapRewriteT Fusion.forgetRewrites
  disable = mapRewriteT Fusion.disable
  isDisabled = Trans.lift Fusion.isDisabled

instance Env.MatchRead m => Env.MatchRead (RewriteT m) where
  matches = Trans.lift Env.matches
  
instance Discovery.Tells m => Discovery.Tells (RewriteT m) where
  tell = Trans.lift . Discovery.tell
  
instance History.Env m => History.Env (RewriteT m) where
  ask = Trans.lift History.ask
  local f = mapRewriteT (History.local f)
  
instance Direction.Has m => Direction.Has (RewriteT m) where
  get = Trans.lift Direction.get
  local d = mapRewriteT (Direction.local d)
  
instance Memo.Can m => Memo.Can (RewriteT m) where
  maybeMemo n t = mapRewriteT instep
    where
    instep :: m (Maybe (Maybe Term)) -> m (Maybe (Maybe Term))
    instep mx =
      liftM return (Memo.maybeMemo n t (liftM join mx))

instance Steps.Counter m => Steps.Counter (RewriteT m) where
  take = Trans.lift Steps.take
  listen = RewriteT . Steps.listen . rewriteT

instance Steps.Limiter m => Steps.Limiter (RewriteT m) where
  limit n = RewriteT . Steps.limit n . rewriteT
  remaining = Trans.lift Steps.remaining

instance Env m => Env (RewriteT m) where
  clearContext = mapRewriteT clearContext
  augmentContext = mapRewriteT . augmentContext
  applyContext = Trans.lift . applyContext
  localStepName = mapRewriteT . localStepName
  askStepName = Trans.lift askStepName

instance TraceSteps.Env m => TraceSteps.Env (RewriteT m) where
  enabled = Trans.lift TraceSteps.enabled
  enable = mapRewriteT TraceSteps.enable

instance Signals.Env m => Signals.Env (RewriteT m) where
  tellStopRewriting = Trans.lift Signals.tellStopRewriting
  tellUsedAntecentRewrite = Trans.lift Signals.tellUsedAntecentRewrite
  consume = mapRewriteT (liftM sequenceFst . Signals.consume)

class (TraceSteps.Env m, Signals.Env m) => Env m where
  applyContext :: Term -> m Term
  augmentContext :: (Term -> Term) -> m a -> m a
  clearContext :: m a -> m a
  localStepName :: String -> m a -> m a
  askStepName :: m String
