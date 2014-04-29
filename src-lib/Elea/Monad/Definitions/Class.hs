module Elea.Monad.Definitions.Class
(
  Read (..), Write (..),
  unfold,
)
where

import Elea.Prelude hiding ( get )
import Elea.Term
import Elea.Definition
import Elea.Monad.Env ()
import qualified Elea.Prelude as Prelude
import qualified Elea.Index as Indices

class Monad m => Read m where
  get :: Name -> m Definition
  
  -- | Attempt to find a definition whose body matches this term
  search :: Term -> m (Maybe Definition)


class Monad m => Write m where
  put :: Name -> Definition -> m ()

-- | Given a function name, and the arguments supplied, this will
-- unfold the definition of the function and apply the arguments.
unfold :: Read m => Name -> [Term] -> m Term
unfold name args = do
  def <- get name 
  return 
    . Indices.substMany args 
    $ Prelude.get body def

