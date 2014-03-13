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
import Elea.Type

type Has m = (Read m, Write m)

class Monad m => Read m where
  -- | Instead of returning 'Polymorphic Term' we take the type arguments 
  -- manually. This is so that this 'Term' with these arguments can be
  -- easily looked up later in 'lookupName'.
  lookupTerm :: String -> [Type] -> m (Maybe Term)
  
  -- | Instead of returning 'Polymorphic Ind' we take the type arguments
  -- so we can rename the 'Ind' to refer to them.
  lookupType :: String -> [Type] -> m (Maybe Ind)
  
  -- | Given a term, attempt to find a name for this term, and the
  -- term arguments which were applied to it. For example:
  -- > termName (append<nat> xs ys) ==> ("append<nat>", [xs, ys])
  lookupName :: Term -> m (Maybe (String, [Term]))
  
class Monad m => Write m where
  defineTerm :: String -> Polymorphic Term -> m ()
  defineType :: String -> Polymorphic Ind -> m ()
  

instance Read m => Read (ReaderT r m) where
  lookupTerm n = lift . lookupTerm n
  lookupType n = lift . lookupType n
  lookupName = lift . lookupName
  
instance Write m => Write (ReaderT r m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (EitherT e m) where
  lookupTerm n = lift . lookupTerm n
  lookupType n = lift . lookupType n
  lookupName = lift . lookupName
  
instance Write m => Write (EitherT e m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (MaybeT m) where
  lookupTerm n = lift . lookupTerm n
  lookupType n = lift . lookupType n
  lookupName = lift . lookupName
  
instance Write m => Write (MaybeT m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n

instance (Monoid w, Read m) => Read (WriterT w m) where
  lookupTerm n = lift . lookupTerm n
  lookupType n = lift . lookupType n
  lookupName = lift . lookupName 
  
instance (Monoid w, Write m) => Write (WriterT w m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
