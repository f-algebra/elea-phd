module Elea.Core
(
  Elea 
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )

import qualified Elea.Definitions as Defs
import qualified Data.Map as Map

type Elea = State (Map String Term)

instance Defs.Monad Elea where
  lookup n = gets (Map.lookup n)
  add n t = modify (Map.insert n t)

