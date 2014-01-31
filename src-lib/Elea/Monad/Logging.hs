module Elea.Monad.Logging
(
  Monad (..)
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad )
import qualified Elea.Prelude as Prelude

class Prelude.Monad m => Monad m where
  info :: String -> m ()

