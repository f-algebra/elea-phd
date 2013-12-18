{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Definitions
(
  Has (..)
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Data.Map as Map
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State

-- | The 'Definitions' 'Monad' can store and load 'Term' definitions.
class Monad m => Has m where
  lookupTerm :: String -> m (Maybe Term)
  defineTerm :: String -> Term -> m ()
  
  lookupType :: String -> m (Maybe Type)
  defineType :: String -> Type -> m ()

instance Has m => Has (ReaderT r m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Has m => Has (EitherT e m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Monad m => 
    Has (StateT (Map String Term, Map String Type) m) where
  lookupTerm n = State.gets (Map.lookup n . fst)
  lookupType n = State.gets (Map.lookup n . snd)
  defineTerm k v = State.modify (first (Map.insert k v))
  defineType k v = State.modify (second (Map.insert k v))

