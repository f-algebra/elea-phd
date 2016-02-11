-- | The Fusion, Environment, Definitions and Discovery monad.
module Elea.Monad.Fedd
(
  FeddT, Fedd,
  evalT, eval,
  setDefinitions,
  getDefinitions,
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.History as History
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Discovery.EquationSet as EqSet
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Definitions.Data as Defs
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Memo.Data as MemoDB
import qualified Elea.Monad.Discovery.Class as Disc
import qualified Elea.Monad.Discovery as Disc
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Env.Data as EnvDB
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.StepCounter as Steps
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Transform.TraceSteps as TraceSteps
import qualified Elea.Monad.Transform.Signals as Signals
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Writer.Class as Writer
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Nat as Nat

-- TODO disable context tracking if both TRACE and ASSERT are off?

type Fedd = FeddT Identity

data FeddState 
  = FS  { _fsDefs :: !Defs.Data
        , _fsMemo :: !MemoDB.Data
        , _fsTagGen :: !Int
        , _fsStepsRemaining :: !CoNat
        , _fsTraceStepsFlag :: !Bool
        , _fsSignals :: !Signals.Signals }

newtype FeddT m a 
  = FeddT { 
    runFeddT :: RWST EnvDB.Data () FeddState m a }
  deriving 
  ( Functor, Applicative 
  , Monad, MonadTrans
  , MonadReader EnvDB.Data
  , MonadState FeddState )
        
mkLabels [ ''FeddState ]

evalT :: Monad m => FeddT m a -> m a
evalT fedd = do
  (x, _, _) <- runRWST (runFeddT fedd) empty empty
  return x

instance Empty FeddState where
  empty = FS empty empty 1 Nat.omega False empty

eval :: Fedd a -> a
eval = runIdentity . evalT

setDefinitions :: Monad m => Defs.Data -> FeddT m ()
setDefinitions = State.modify . set fsDefs

getDefinitions :: Monad m => FeddT m Defs.Data
getDefinitions = State.gets (get fsDefs)

instance Runnable FeddT where
  runM = evalT
  
instance Monad m => Env.Write (FeddT m) where
  bindAt at b = local (EnvDB.bindAt at b)
  matched m = local (EnvDB.matched m)
  forgetMatches w = local (EnvDB.forgetMatches w)

instance Monad m => Env.Read (FeddT m) where
  bindings = asks EnvDB.bindings
  
instance Monad m => Env.MatchRead (FeddT m) where
  matches = asks EnvDB.matches
  
instance Monad m => Disc.Tells (FeddT m) where
  tell _ = return ()

instance Monad m => Disc.Listens (FeddT m) where
  listen = liftM (\x -> (x, [])) 
  
instance Monad m => Defs.Write (FeddT m) where
  defineTerm n t = State.modify (modify fsDefs (Defs.putTerm n t))
  defineType n ty = State.modify (modify fsDefs (Defs.putType n ty))
  
instance Monad m => Defs.Read (FeddT m) where
  lookupName n = State.gets (Defs.getName n . get fsDefs)
  lookupType n tys = State.gets (Defs.getType n tys . get fsDefs)
  lookupTerm n tys = runMaybeT getTerm
    where
    getTerm :: MaybeT (FeddT m) Term
    getTerm = do
      db <- State.get
      (t, def_db') <- Defs.getTerm n tys (get fsDefs db)
      State.put (set fsDefs def_db' db)
      return t

      
instance Monad m => Memo.Can (FeddT m) where
  maybeMemo name term cont = do
    memo_db <- State.gets (get fsMemo)
    case MemoDB.lookup name term memo_db of
      Nothing -> do
        mby_t <-  cont
        let memo_db' = MemoDB.insert name term mby_t memo_db
        -- trace ("\n\n\n\n!!!!! FAILED !!!!!\n\n\n" ++ show term ++ "\n\nDB:" ++ show memo_db')
        State.modify (set fsMemo memo_db')
        return mby_t
        
      Just memo_t -> do
        -- trace ("\n[memoised]\n") 
        return memo_t
       -- mby_t' <- cont
       -- if isJust mby_t' && memo_t /= mby_t'
        -- then error (show memo_t ++ "\n\nactually\n\n" ++ show mby_t')
       -- else return memo_t

instance Monad m => Fusion.Env (FeddT m) where
  rewrites = asks EnvDB.rewrites
  local a t x = local (EnvDB.addRewrite a t x) 
  forgetRewrites = local EnvDB.forgetRewrites
  disable = local (set EnvDB.disableFlag True)
  isDisabled = asks (get EnvDB.disableFlag)
    
instance Monad m => History.Env (FeddT m) where
  ask = asks (get EnvDB.history)
  local f = local (modify EnvDB.history f)
  
instance Monad m => Tag.Gen (FeddT m) where
  generateId = do
    new_i <- State.gets (get fsTagGen)
    State.modify (set fsTagGen (new_i + 1))
    return new_i
  
instance Monad m => Direction.Has (FeddT m) where
  get = asks (get EnvDB.direction)
  local d = local (set EnvDB.direction d)

instance Monad m => Steps.Counter (FeddT m) where
  take = return ()
  listen = liftM (\x -> (x, 0))

instance Monad m => Steps.Limiter (FeddT m) where
  limit n continue = do
    -- Acts like a Reader here, but needs to be a State for Steps.take to work
    prev_limit <- Steps.remaining
    State.modify (set fsStepsRemaining (enum n))
    ret_val <- continue
    State.modify (set fsStepsRemaining prev_limit)
    return ret_val

  remaining = State.gets (get fsStepsRemaining)

instance MonadIO m => MonadIO (FeddT m) where
  liftIO = Trans.lift . liftIO

instance Monad m => Transform.Env (FeddT m) where
  applyContext term = asks (EnvDB.applyContext term)
  augmentContext with = local (EnvDB.augmentContext with)
  clearContext = local EnvDB.clearContext
  localStepName name = local (set EnvDB.stepName name)
  askStepName = asks (get EnvDB.stepName)

instance Monad m => TraceSteps.Env (FeddT m) where
  enabled = asks (get EnvDB.traceStepsFlag)
  enable = local (set EnvDB.traceStepsFlag True)

instance Monad m => Signals.Env (FeddT m) where
  tellStopRewriting =
    State.modify (set (Signals.stopRewriting . fsSignals) True)
  tellUsedAntecentRewrite = 
    State.modify (set (Signals.usedAntecedentRewrite . fsSignals) True)

  consume run = do
    x <- run
    signals <- State.gets (get fsSignals)
    State.modify (set fsSignals empty)
    return (x, signals)
