-- | Useful instances for the monad classes in "Elea.Monad.Discovery".
module Elea.Monad.Discovery
(
  module Elea.Monad.Discovery.Class,
  
  IgnoreT, Ignore,
  ignoreT, ignore,
  
  ListenerT, Listener,
  listener, listenerT,
  traceT, trace,
)
where

import Prelude ()
import Elea.Prelude hiding ( tell, listen, trace )
import Elea.Term
import Elea.Show
import Elea.Monad.Discovery.Class
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Env as Env
import qualified Control.Monad.Writer as Writer

-- | A monad which ignores discoveries it is passed
newtype IgnoreT m a 
  = IgnoreT { ignoreT :: m a }
  deriving ( Monad )
  
type Ignore = IgnoreT Identity

ignore :: Ignore a -> a
ignore = runIdentity . ignoreT

instance MonadTrans IgnoreT where 
  lift = IgnoreT

instance Tells Ignore where
  tell _ = return ()

  
-- | A set of equations which have been discovered.
-- Its 'Monoid' instance is important, as this decides how equations subsume
-- or combine with each other. 
-- Right now I've given it a temporary trivial definition.
newtype EquationSet
  = EqSet { runEqSet :: [Equation] }
  
instance Monoid EquationSet where
  mempty = EqSet []
  mappend (EqSet es1) (EqSet es2) = 
    EqSet (es1 ++ es2)
    

-- | A monad to record discoveries
newtype ListenerT m a 
  = ListenerT { runListenerT :: WriterT EquationSet m a }
  deriving ( Monad, MonadTrans )
  
type Listener = ListenerT Identity
  
listenerT :: Monad m => ListenerT m a -> m (a, [Equation])
listenerT = liftM (second runEqSet) . runWriterT . runListenerT

listener :: Listener a -> (a, [Equation])
listener = runIdentity . listenerT

traceT :: Monad m => ListenerT m a -> m a
traceT run = do
  (x, eqs) <- listenerT run
  traceEqs eqs (return x)
  where
  traceEqs eqs = Prelude.trace
    $ "[Discovered Equations]\n\n" 
    ++ intercalate "\n" (map show eqs)

trace :: Listener a -> a
trace = runIdentity . traceT

instance Monad m => Tells (ListenerT m) where
  tell eq = ListenerT (Writer.tell (EqSet [eq]))
  
instance Monad m => Listens (ListenerT m) where
  listen = ListenerT . liftM (second runEqSet) . Writer.listen . runListenerT
