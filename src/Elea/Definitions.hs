module Elea.Definitions
(
  Monad (..)
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, lookup )
import Elea.Term ( Term )
import qualified Elea.Prelude as Prelude

-- | The 'Definitions' 'Monad' can store and load 'Term' definitions
class Prelude.Monad m => Monad m where
  lookup :: String -> m (Maybe Term)
  add :: String -> Term -> m ()

instance Monad m => Monad (ReaderT r m) where
  lookup = lift . lookup
  add n = lift . add n
