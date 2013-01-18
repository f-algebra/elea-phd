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
import qualified Elea.Term as Term

-- | The 'Definitions' 'Monad' can store and load 'Term' definitions.
-- Parameterised by the type of term annotations.
class (Term.Notes a, Prelude.Monad m) => Monad a m | m -> a where
  lookup :: String -> m (Maybe (Term a))
  add :: String -> Term a -> m ()

instance Monad a m => Monad a (ReaderT r m) where
  lookup = lift . lookup
  add n = lift . add n
  
instance Monad a m => Monad a (EitherT e m) where
  lookup = lift . lookup
  add n = lift . add n
