module Elea.Monad.History
(
  Repr,
  Env (..),
  EnvT,
  emptyEnvT,
  mapEnvT,
  check,
)
where

import Elea.Prelude hiding ( ask )
import Elea.Embed ( Code, Encodable )
import qualified Elea.Embed as Embed
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans 
import qualified Data.Poset as Partial

type Repr = [Code]

class Monad m => Env m where
  seeCode :: Code -> m a -> m a
  ask :: m Repr
  
  -- | Use our partial order on codes to check for embedding
  seenCode :: Code -> m Bool
  seenCode c =
    liftM (any ((Partial.>) c)) ask

  seen :: Encodable a => a -> m Bool
  seen = seenCode . Embed.encode
  
  see :: Encodable a => a -> m b -> m b
  see = seeCode . Embed.encode
  
  
check :: (Fail.Can m, Env m, Encodable a) => a -> m b -> m b
check x continue = do
  fail <- seen x
  Fail.when fail
  see x continue
  

newtype EnvT m a
  = EnvT { envT :: ReaderT [Code] m a }
  deriving ( Functor, Monad, MonadReader [Code], MonadTrans )

emptyEnvT :: EnvT m a -> m a
emptyEnvT = flip runReaderT mempty  . envT

mapEnvT :: (m a -> n b) -> EnvT m a -> EnvT n b
mapEnvT f = EnvT . mapReaderT f . envT
  
instance Monad m => Env (EnvT m) where
  ask = Reader.ask
  seeCode code =
    Reader.local (\codes -> code : codes)
    
instance Fail.Can m => Fail.Can (EnvT m) where
  here = Trans.lift Fail.here
  catch = mapEnvT Fail.catch
  
instance Env m => Env (MaybeT m) where
  ask = Trans.lift ask
  seeCode x = mapMaybeT (seeCode x)
  

