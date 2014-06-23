module Elea.Monad.Definitions.Database
(
  Database,
  empty,
  lookup,
  insert
)
where

import Elea.Prelude hiding ( empty, lookup )
import Elea.Term
import Elea.Definition
import qualified Data.Map as Map

newtype Database 
  = Database    { defs :: Map Name (Poly Definition) }

lookup :: Name -> Database -> Poly Definition
lookup n = fromJust . Map.lookup n . defs

insert :: Name -> Poly Definition -> Database -> Database
insert n d db = db { defs = Map.insert n d (defs db) }

empty :: Database
empty = Database Map.empty

