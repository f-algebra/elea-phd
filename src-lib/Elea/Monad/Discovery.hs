-- | The discovery monad classes are 'MonadWriter' for equations.
module Elea.Monad.Discovery
(
  Makes (..), Listens (..), 
  IgnoreT, Ignore,
  ignoreT, ignore,
  equals, run
)
where

import Prelude ()
import Elea.Prelude hiding ( tell, listen )
import Elea.Index hiding ( lift )
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Control.Monad.Writer as Writer

class Monad m => Makes m where
  tell :: Equation -> m ()
  
class Makes m => Listens m where
  listen :: m a -> m (a, [Equation])

equals :: (Env.Read m, Makes m) => Term -> Term -> m ()
equals t1 t2 = do
  bs <- Env.bindings
  tell (Equals "" bs t1 t2)

instance Makes m => Makes (MaybeT m) where
  tell = lift . tell
  
newtype EquationSet
  = EqSet { runEqSet :: [Equation] }
  
instance Monoid EquationSet where
  mempty = EqSet []
  mappend (EqSet es1) (EqSet es2) = 
    EqSet (es1 ++ es2)

instance Monad m => Makes (WriterT EquationSet m) where
  tell eq = Writer.tell (EqSet [eq])
  
instance Monad m => Listens (WriterT EquationSet m) where
  listen = liftM (second runEqSet) . Writer.listen
  
instance Makes Ignore where
  tell _ = return ()
  
newtype IgnoreT m a 
  = IgnoreT { ignoreT :: m a }
  deriving ( Monad )
  
type Ignore = IgnoreT Identity

ignore :: Ignore a -> a
ignore = runIdentity . ignoreT
  
run :: Monad m => WriterT EquationSet m a -> m (a, [Equation])
run = liftM (second runEqSet) . runWriterT

