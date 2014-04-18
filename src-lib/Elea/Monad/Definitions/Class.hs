module Elea.Monad.Definitions.Class
(
  Read (..), Write (..),
)
where

import Elea.Prelude
import Elea.Term

class Monad m => Read m where
  get :: Name -> m Term

class Monad m => Write m where
  put :: Name -> Term -> m ()

