-- | The definitions monads provide the means to define terms and types
-- by name, and look them up later.
module Elea.Monad.Definitions
(
  Has, Read (..), Write (..)
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import qualified Data.Map as Map
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State

type Has m = (Read m, Write m)

class Monad m => Read m where
  lookupTerm :: String -> m (Maybe Term)
  lookupType :: String -> m (Maybe Type)
  
  termName :: Term -> m (Maybe String)
  typeName :: Type -> m (Maybe String)
  
class Monad m => Write m where
  defineTerm :: String -> Term -> m ()
  defineType :: String -> Type -> m ()

instance Read m => Read (ReaderT r m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  termName = lift . termName
  typeName = lift . typeName
  
instance Write m => Write (ReaderT r m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (EitherT e m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  termName = lift . termName
  typeName = lift . typeName
  
instance Write m => Write (EitherT e m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Monad m => 
    Read (StateT (Map String Term, Map String Type) m) where
  lookupTerm n = State.gets (Map.lookup n . fst)
  lookupType n = State.gets (Map.lookup n . snd)
  
  termName _ = return Nothing
  typeName _ = return Nothing
  
instance Monad m =>
    Write (StateT (Map String Term, Map String Type) m) where
  defineTerm k v = State.modify (first (Map.insert k v))
  defineType k v = State.modify (second (Map.insert k v))

