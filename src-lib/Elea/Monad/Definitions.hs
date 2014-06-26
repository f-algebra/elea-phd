-- | A concrete implementation of a database of term definitions (let bindings),
-- along with a reader and state monad just for this database type.
module Elea.Monad.Definitions
(
  module Elea.Monad.Definitions.Class,
  module Elea.Monad.Definitions.Database,

  DBReaderT, DBReader,
  readEmptyT, readEmpty,
  
  DBStateT, DBState,
  evalEmptyT, evalEmpty
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Monad.Definitions.Class
import Elea.Monad.Definitions.Database ( Database )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.Definitions.Database as DB
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Class as Trans

newtype DBReaderT m a 
  = DBReaderT { runDBReaderT :: ReaderT Database m a }
  deriving ( Monad, MonadTrans, MonadReader Database )
  
type DBReader = DBReaderT Identity

readEmptyT :: Monad m => DBReaderT m a -> m a
readEmptyT = flip runReaderT DB.empty . runDBReaderT

readEmpty :: DBReader a -> a
readEmpty = runIdentity . readEmptyT

instance (Show Term, Monad m) => Read (DBReaderT m) where
  lookupTerm n = asks . DB.getTerm' n
  lookupType n = asks . DB.getType n
  lookupName = asks . DB.getName
  
newtype DBStateT m a 
  = DBStateT { runDBStateT :: StateT Database m a }
  deriving ( Monad, MonadTrans, MonadState Database )
  
type DBState = DBStateT Identity
  
evalEmptyT :: Monad m => DBStateT m a -> m a
evalEmptyT = flip evalStateT DB.empty . runDBStateT

evalEmpty :: DBState a -> a 
evalEmpty = runIdentity . evalEmptyT

mapDBStateT :: ()
  => (m (a, Database) -> n (b, Database)) 
  -> (DBStateT m a -> DBStateT n b)
mapDBStateT f = DBStateT . mapStateT f . runDBStateT

instance (Show Term, Monad m) => Read (DBStateT m) where
  lookupType n = State.gets . DB.getType n
  lookupTerm n = runMaybeT . DB.stateGetTerm n
  lookupName = State.gets . DB.getName
      
instance (Show Term, Monad m) => Write (DBStateT m) where
  defineTerm n = State.modify . DB.putTerm n
  defineType n = State.modify . DB.putType n
  
instance Env.Write m => Env.Write (DBStateT m) where
  bindAt at b = mapDBStateT (Env.bindAt at b)
  matched t w = mapDBStateT (Env.matched t w)
  
instance Env.Bindings m => Env.Bindings (DBStateT m) where
  bindings = Trans.lift Env.bindings
  
instance Discovery.Tells m => Discovery.Tells (DBStateT m) where
  tell = Trans.lift . Discovery.tell

instance Discovery.Listens m => Discovery.Listens (DBStateT m) where
  listen = mapDBStateT (liftM swap . Discovery.listen)
    where
    swap :: ((a, b), c) -> ((a, c), b)
    swap ((x, y), z) = ((x, z), y)
