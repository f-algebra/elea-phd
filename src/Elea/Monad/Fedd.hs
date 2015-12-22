-- | The Fusion, Environment, Definitions and Discovery monad.
-- Possible speed increase by collapsing a monad transformer stack.
module Elea.Monad.Fedd
(
  FeddT, Fedd,
  evalT, eval,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show
import qualified Elea.Type.Ext as Type
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
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State.Class as State

newtype FeddT m a 
  = FeddT { 
    runFeddT :: RWST EnvDB.Data EqSet.EqSet FeddState m a }
  deriving 
  ( Functor, Applicative 
  , Monad, MonadTrans
  , MonadReader EnvDB.Data
  , MonadWriter EqSet.EqSet
  , MonadState FeddState )
  
type Fedd = FeddT Identity

data FeddState 
  = FS  { _fsDefs :: !Defs.Data
        , _fsMemo :: !MemoDB.Data
        , _fsTagGen :: !Int }
        
mkLabels [ ''FeddState ]

evalT :: Monad m => FeddT m a -> m a
evalT fedd = do
  (x, _, _) <- runRWST (runFeddT fedd) EnvDB.empty emptyState
  return x
    

emptyState :: FeddState
emptyState = FS Defs.empty MemoDB.empty 1
  
eval :: Fedd a -> a
eval = runIdentity . evalT
  
instance Monad m => Env.Write (FeddT m) where
  bindAt at b = local (EnvDB.bindAt at b)
  matched m = local (EnvDB.matched m)
  forgetMatches w = local (EnvDB.forgetMatches w)
  
instance Monad m => Env.Read (FeddT m) where
  bindings = asks EnvDB.bindings
  
instance Monad m => Env.MatchRead (FeddT m) where
  matches = asks EnvDB.matches
  
instance Monad m => Disc.Tells (FeddT m) where
  tell = tell . EqSet.singleton
  
instance Monad m => Disc.Listens (FeddT m) where
  listen = liftM (second EqSet.toList) . listen
  
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