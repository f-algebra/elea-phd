{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Definitions
(
  Monad (..)
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, lookup )
import Elea.Term ( Term )
import qualified Elea.Prelude as Prelude
import qualified Data.Map as Map
import qualified Control.Monad.State as State

-- | The 'Definitions' 'Monad' can store and load 'Term' definitions.
class Prelude.Monad m => Monad m where
  lookup :: String -> m (Maybe Term)
  add :: String -> Term -> m ()

instance Monad m => Monad (ReaderT r m) where
  lookup = lift . lookup
  add n = lift . add n
  
instance Prelude.Monad m => Monad (StateT (Map String Term) m) where
  lookup n = State.gets (Map.lookup n)
  add k v = State.modify (Map.insert k v)

instance Monad m => Monad (EitherT e m) where
  lookup = lift . lookup
  add n = lift . add n
