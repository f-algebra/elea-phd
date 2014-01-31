-- | The definitions monads provide the means to define terms and types
-- by name, and look them up later.
module Elea.Monad.Definitions
(
  Has, Read (..), Write (..),
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term

type Has m = (Read m, Write m)

class Monad m => Read m where
  lookupTerm :: String -> m (Maybe Term)
  lookupType :: String -> m (Maybe Type)
  
  -- | Given a term, attempt to find a name for this term, provided the name
  -- is given the list of terms as arguments.
  -- E.g. "termName (append xs ys) ==> ('append', [xs, ys])"
  lookupName :: Term -> m (Maybe (String, [Term]))
  
class Monad m => Write m where
  defineTerm :: String -> Term -> m ()
  defineType :: String -> Type -> m ()

instance Read m => Read (ReaderT r m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (ReaderT r m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (EitherT e m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (EitherT e m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (MaybeT m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (MaybeT m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n

