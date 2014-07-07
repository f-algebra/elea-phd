-- | The Fusion, Environment, Definitions and Discovery monad.
-- Possible speed increase by collapsing a monad transformer stack.
-- Unfinished.
module Elea.Monad.Fedd
(
  FeddT, Fedd,
  evalT, eval,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show
import Elea.Monad.Fusion.Data ( Outcome (..) )
import qualified Elea.Context as Context
import qualified Elea.Monad.Discovery.EquationSet as EqSet
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Definitions.Data as Defs
import qualified Elea.Monad.Fusion.Class as Fusion
import qualified Elea.Monad.Fusion.Data as FusionDB
import qualified Elea.Monad.Discovery.Class as Disc
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Env.Data as EnvDB
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State.Class as State

newtype FeddT m a 
  = FeddT { 
    runFeddT :: RWST EnvDB.Data EqSet.EqSet (Defs.Data, FusionDB.Data) m a }
  deriving 
  ( Monad, MonadTrans
  , MonadReader EnvDB.Data
  , MonadWriter EqSet.EqSet
  , MonadState (Defs.Data, FusionDB.Data) )
  
type Fedd = FeddT Identity

evalT :: Monad m => FeddT m a -> m a
evalT = id
  . liftM fst3
  . (\rwst -> runRWST rwst EnvDB.empty (Defs.empty, FusionDB.empty))
  . runFeddT
  where
  fst3 (x, _, _) = x
  
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
  defineTerm n t = State.modify (first (Defs.putTerm n t))
  defineType n ty = State.modify (first (Defs.putType n ty))
  
instance Monad m => Defs.Read (FeddT m) where
  lookupName n = State.gets (Defs.getName n . fst)
  lookupType n tys = State.gets (Defs.getType n tys . fst)
  lookupTerm n tys = runMaybeT getTerm
    where
    getTerm :: MaybeT (FeddT m) Term
    getTerm = do
      (def_db, f_db) <- State.get
      (t, def_db') <- Defs.getTerm n tys def_db
      State.put (def_db', f_db)
      return t
  
-- Memoise our fixpoint fusion calculations to improve speed
-- and stop potential loops.
instance Monad m => Fusion.Memo (FeddT m) where
  memoiseMaybe ctx term run = do
    fusion_db <- State.gets snd
    case FusionDB.lookup orig_t fusion_db of
      -- If we are in the process of fusing this then we need to fail
      -- to prevent infinite loops
      Just Pending -> id
        . trace ("[FusionDB] stopped a loop") 
        $ return Nothing
      
      -- If fusion failed previously then it will fail again
      Just Failure -> id
        . trace ("[FusionDB] improved efficiency (failure)") 
        $ return Nothing
      
      -- If fusion succeeded previously then we can be efficient 
      -- and return the previous result
      Just (Success t) -> id
        . trace ("[FusionDB] improved efficiency (success)") 
        $ return (Just t)
      
      -- If we have not yet tried this fusion then...
      Nothing -> do
        -- Set that we are performing this fusion
        let fusion_db' = FusionDB.insert orig_t Pending fusion_db
        State.modify (second (const fusion_db'))
        
        -- Run the fusion step
        mby_t <- run
        
        -- Store the result of this fusion
        let fusion_db'' =
              FusionDB.insert orig_t (FusionDB.maybeToOutcome mby_t) fusion_db
        State.modify (second (const fusion_db''))
        return mby_t
    where
    orig_t = Context.apply ctx term
    
