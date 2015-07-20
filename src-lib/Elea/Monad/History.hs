{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.History
(
  Repr, 
  Name,
  Env (..),
  EnvT,
  empty,
  size,
  embeds,
  insert,
  emptyEnvT,
  mapEnvT,
  check,
  memoCheck,
)
where

import Elea.Prelude hiding ( ask, local )
import Elea.Transform.Names ( Name )
import Elea.Term
import qualified Elea.Monad.Failure.Class as Fail 
import qualified Elea.Monad.Memo.Class as Memo
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Poset as Quasi
import qualified Data.IntMap as Map

data Repr 
  = Repr  { _reprSize :: !Nat
          , _reprMap :: !(IntMap [Term]) }
          
instance Show Term => Show Repr where
  show = show . _reprMap
          
mkLabels [ ''Repr ]

embeds :: Name -> Term -> Repr -> Bool
embeds (fromEnum -> name) t repr = 
  case Map.lookup name (get reprMap repr) of
    Nothing -> False
    Just codes -> any (Quasi.<= t) codes
    
insert :: Name -> Term -> Repr -> Repr
insert (fromEnum -> name) t = id
  . modify reprSize (+ 1)
  . modify reprMap (Map.alter add name)
  where
  add (Just ts) = Just (t:ts)
  add Nothing = Just [t]
  
empty :: Repr
empty = Repr 0 Map.empty

size :: Repr -> Nat
size = get reprSize

forgetName :: Name -> Repr -> Repr
forgetName k = modify reprMap (Map.delete (enum k))

class Monad m => Env m where
  ask :: m Repr
  local :: (Repr -> Repr) -> m a -> m a
  
  see :: Name -> Term -> m a -> m a
  see name t = local (insert name t)
  
  -- | Use our partial order on codes to check for embedding.
  -- The first argument is the step name that invoked this.
  seen :: Name -> Term -> m Bool
  seen name t = liftM (embeds name t) ask
  
  forget :: Name -> m a -> m a
  forget k = local (forgetName k)
  
  
check :: (Fail.Can m, Env m) => Name -> Term -> m b -> m b
check name t continue = do
  fail <- seen name t
  Fail.when fail
  see name t continue
  
-- | History check + memoisation
memoCheck :: (Fail.Can m, Env m, Memo.Can m) => Name -> Term -> m Term -> m Term
memoCheck name t = id
  . check name t
  . Memo.memo name t 
   

newtype EnvT m a
  = EnvT { envT :: ReaderT Repr m a }
  deriving ( Functor, Monad, MonadReader Repr, MonadTrans )

emptyEnvT :: EnvT m a -> m a
emptyEnvT = flip runReaderT empty  . envT

mapEnvT :: (m a -> n b) -> EnvT m a -> EnvT n b
mapEnvT f = EnvT . mapReaderT f . envT
  
instance Monad m => Env (EnvT m) where
  ask = Reader.ask
  local = Reader.local
    
instance Fail.Can m => Fail.Can (EnvT m) where
  here = Trans.lift Fail.here
  catch = mapEnvT Fail.catch
  
instance Env m => Env (MaybeT m) where
  ask = Trans.lift ask
  local f = mapMaybeT (local f)
  
instance Env m => Env (IdentityT m) where
  ask = Trans.lift ask
  local f = mapIdentityT (local f)
  

