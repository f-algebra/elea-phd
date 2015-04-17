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
import Elea.Monad.Memo.Data ( Outcome (..) )
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.History as History
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Monad.Discovery.EquationSet as EqSet
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Definitions.Data as Defs
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Memo.Data as MemoDB
import qualified Elea.Monad.Discovery.Class as Disc
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Env.Data as EnvDB
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State.Class as State

newtype FeddT m a 
  = FeddT { 
    runFeddT :: RWST EnvDB.Data EqSet.EqSet FeddState m a }
  deriving 
  ( Monad, MonadTrans
  , MonadReader EnvDB.Data
  , MonadWriter EqSet.EqSet
  , MonadState FeddState )
  
type Fedd = FeddT Identity

data FeddState 
  = FS  { _fsDefs :: !Defs.Data
        , _fsFusions :: !MemoDB.Data
        , _fsConstraintFusions :: !MemoDB.Data
        , _fsTagGen :: !Int }
        
mkLabels [ ''FeddState ]

evalT :: Monad m => FeddT m a -> m a
evalT = id
  . liftM fst3
  . (\rwst -> runRWST rwst EnvDB.empty emptyState)
  . runFeddT
  where
  fst3 (x, _, _) = x

emptyState :: FeddState
emptyState = FS Defs.empty MemoDB.empty MemoDB.empty 0
  
eval :: Fedd a -> a
eval = runIdentity . evalT
  
instance Monad m => Env.Write (FeddT m) where
  bindAt at b = local (EnvDB.bindAt at b)
  matched t1 t2 = local (EnvDB.matched t1 t2)
  
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
  
-- Memoise our fixpoint fusion calculations to improve speed
-- and stop potential loops.
memoise :: Monad m 
  => (FeddState :-> MemoDB.Data)
  -> Term 
  -> FeddT m (Maybe Term) 
  -> FeddT m (Maybe Term)
memoise lens term run = do
  fusion_db <- State.gets (get lens)
  case MemoDB.lookup term fusion_db of
    -- If we are in the process of fusing this then we need to fail
    -- to prevent infinite loops
    Just Pending -> id
     -- . trace ("[memo] stopped a loop") 
      $ return Nothing
    
    -- If fusion failed previously then it will fail again
    Just Failure -> id
    --  . trace ("[memo] improved efficiency (failure)") 
      $ return Nothing
    
    -- If fusion succeeded previously then we can be efficient 
    -- and return the previous result
    Just (Success t) -> id
     -- . trace ("[memo] improved efficiency (success)")
      $ return (Just t)
    
    -- If we have not yet tried this fusion then...
    Nothing -> do
      -- Set that we are performing this fusion
      let fusion_db' = MemoDB.insert term Pending fusion_db
      State.modify (modify lens (const fusion_db'))
      
      -- Run the fusion step
      mby_t <- run
      let outcome = MemoDB.maybeToOutcome mby_t
      
      -- Store the result of this fusion
      let fusion_db'' =
            MemoDB.insert term outcome fusion_db
      State.modify (modify lens (const fusion_db''))
      return mby_t

      {-
instance Monad m => Memo.Can (FeddT m) where
  maybeFusion ctx term = 
    memoise fsFusions (Context.apply ctx term)
  
  maybeConstraintFusion cons term = 
    memoise fsConstraintFusions (Context.apply ctx term)
    where
    ctx = Constraint.manyToContext (Type.get term) cons
    
  maybeFission _ _ = id
  -}
  
instance Monad m => Rewrite.Env (FeddT m) where
  rewrites = asks EnvDB.rewrites
  local a t x = local (EnvDB.addRewrite a t x) 
  
    
instance Monad m => History.Env (FeddT m) where
  ask = asks (get EnvDB.history)
  local f = local (modify EnvDB.history f)
  
instance Monad m => Tag.Gen (FeddT m) where
  generateId = do
    new_i <- State.gets (get fsTagGen)
    State.modify (set fsTagGen (new_i + 1))
    return new_i
  
