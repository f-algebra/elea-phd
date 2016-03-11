-- | The discovery monad classes are 'MonadWriter' for equations.
module Elea.Monad.Discovery.Class
(
  Tells (..), Listens (..), 
  rewritesTo, rewritesToM,
)
where

import Elea.Prelude hiding ( tell, listen )
import Elea.Term
import qualified Elea.Monad.Env.Class as Env
import qualified Control.Monad.Writer as Writer

class Monad m => Tells m where
  tell :: Prop -> m ()
  
  tellAll :: [Prop] -> m ()
  tellAll = mapM_ tell
  
class Tells m => Listens m where
  listen :: m a -> m (a, [Prop])

rewritesTo :: (Env.Read m, Tells m) => Term -> Term -> m ()
rewritesTo t1 t2 = do
  bs <- Env.bindings
  tell (Prop "" (unflattenLam (reverse bs) (Leq t1 t2)) True)
  
rewritesToM :: (Env.Read m, Tells m) => Term -> m Term -> m Term
rewritesToM t1 mt2 = do
  t2 <- mt2
  rewritesTo t1 t2
  return t2

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

