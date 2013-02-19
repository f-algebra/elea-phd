{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Definitions
(
  Monad (..)
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, lookup )
import Elea.Type ( Type )
import Elea.Term ( Term )
import qualified Elea.Prelude as Prelude

-- | The 'Definitions' 'Monad' can store and load 
-- 'Term' and 'Type' definitions.
class Prelude.Monad m => Monad m where
  lookupTerm :: String -> m (Maybe Term)
  lookupType :: String -> m (Maybe Type)
  defineTerm :: String -> Term -> m ()
  defineType :: String -> Type -> m ()

instance Monad m => Monad (ReaderT r m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n

