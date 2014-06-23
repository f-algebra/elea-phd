module Elea.Monad.Definitions.Class
(
  Read (..), Write (..),
  unfold
)
where

import Elea.Prelude hiding ( get )
import Elea.Term
import Elea.Definition
import Elea.Monad.Env ()
import qualified Elea.Prelude as Prelude
import qualified Elea.Index as Indices
import qualified Elea.Type as Type

class Monad m => Read m where
  get :: Name -> m (Poly Definition)
  
  getInstance :: Inst Name -> m (Inst Definition)
  getInstance = Type.applyM get
  
  -- | Attempt to find a definition whose body matches this term,
  -- along with the arguments (type and term) to be applied to it
  -- to yield the given term.
  search :: Term -> m (Maybe (Definition, [Type], [Term]))


class Monad m => Write m where
  put :: Poly Name -> [Bind] -> Term -> m ()
  
  
unfold :: Read m => Inst Name -> [Term] -> m Term
unfold iname args = do
  Definition { body = body, bindings = bs } <- id
    . liftM instObj
    $ getInstance iname
  return 
    . assert (nlength bs == length args)
    -- Substitute the type arguments in, the substitute the term arguments.
    $ Indices.substMany args body
  
    
instance Write m => Write (ReaderT r m) where
  put n bs = lift . put n bs

