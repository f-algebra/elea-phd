-- | The discovery monad classes are 'MonadWriter' for equations.
module Elea.Monad.Discovery.Class
(
  Tells (..), Listens (..), 
  equals,
)
where

import Prelude ()
import Elea.Prelude hiding ( tell, listen )
import Elea.Term
import qualified Elea.Monad.Env.Class as Env
import qualified Control.Monad.Writer as Writer

class Monad m => Tells m where
  tell :: Equation -> m ()
  
class Tells m => Listens m where
  listen :: m a -> m (a, [Equation]) 

equals :: (Env.Read m, Tells m) => Term -> Term -> m ()
equals t1 t2 = do
  bs <- Env.bindings
  tell (Equals "" bs t1 t2)

instance Tells m => Tells (MaybeT m) where
  tell = lift . tell
  
instance Listens m => Listens (MaybeT m) where
  listen = mapMaybeT (liftM liftMaybe . listen)
    where
    liftMaybe (Nothing, _) = Nothing
    liftMaybe (Just x, eqs) = Just (x, eqs)
    
instance Tells m => Tells (ReaderT r m) where
  tell = lift . tell
  
instance Listens m => Listens (ReaderT r m) where
  listen = mapReaderT listen


  

