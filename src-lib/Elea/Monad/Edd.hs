-- | The Environment, Definitions and Discovery monad.
-- Possible speed increase by collapsing a monad transformer stack.
-- Unfinished.
module Elea.Monad.Edd
(
  EddT, Edd,
  evalT, eval,
  
  ReddT, Redd,
  readonly
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Discoveries ( EquationSet )
import Elea.Show
import qualified Elea.Discoveries as EqSet
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Definitions.Database as Defs
import qualified Elea.Monad.Discovery.Class as Disc
import qualified Elea.Monad.Env.Class as Env
import qualified Control.Monad.RWS.Lazy as RWS
import qualified Control.Monad.State.Class as State

newtype EddT m a 
  = EddT { runEddT :: RWST [Bind] EquationSet Defs.Database m a }
  deriving 
  ( Monad, MonadTrans
  , MonadReader [Bind]
  , MonadWriter EquationSet
  , MonadState Defs.Database ) 
  
type Edd = EddT Identity

evalT :: Monad m => EddT m a -> m a
evalT = id
  . liftM fst3
  . (\rwst -> runRWST rwst [] Defs.empty)
  . runEddT
  where
  fst3 (x, _, _) = x
  
eval :: Edd a -> a
eval = runIdentity . evalT


newtype ReddT m a
  = ReddT { runReddT :: RWST ([Bind], Defs.Database) EquationSet () m a }
  deriving 
  ( Monad, MonadTrans
  , MonadReader ([Bind], Defs.Database) 
  , MonadWriter EquationSet )
  
type Redd = ReddT Identity

readonly :: (Env.Read m, MonadState Defs.Database m, Disc.Tells m) => Redd a -> m a
readonly read = do
  db <- State.get
  bs <- Env.bindings
  let (a, _, eqs) = id
        . runIdentity
        $ runRWST (runReddT read) (bs, db) ()
 -- Disc.tellAll (EqSet.toList eqs)
  return a
  
instance Monad m => Env.Write (EddT m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ _ = id
  
instance Monad m => Env.Read (EddT m) where
  bindings = ask
  
instance Monad m => Disc.Tells (EddT m) where
  tell = tell . EqSet.singleton
  
instance Monad m => Disc.Listens (EddT m) where
  listen = liftM (second EqSet.toList) . listen
  
instance Monad m => Disc.Tells (ReddT m) where
  tell = tell . EqSet.singleton
  
instance Monad m => Disc.Listens (ReddT m) where
  listen = liftM (second EqSet.toList) . listen
  
instance Monad m => Defs.Write (EddT m) where
  defineTerm n = State.modify . Defs.putTerm n
  defineType n = State.modify . Defs.putType n
  
instance Monad m => Defs.Read (EddT m) where
  lookupName = State.gets . Defs.getName
  lookupType n = State.gets . Defs.getType n
  lookupTerm n = runMaybeT . Defs.stateGetTerm n
  
instance Monad m => Env.Write (ReddT m) where
  matched _ _ = id
  bindAt at b = local (first (insertAt (enum at) b))
  
instance Monad m => Env.Read (ReddT m) where
  bindings = asks fst
  
instance Monad m => Defs.Read (ReddT m) where
  lookupName n = asks (Defs.getName n . snd)
  lookupType n tys = asks (Defs.getType n tys . snd)
  lookupTerm n tys = asks (Defs.getTerm' n tys . snd)
  
