module Elea.Monad.Parser.Database
(
  Database,
  empty,
  lookupTerm,
  lookupInd,
  defineTerm,
  defineInd,
)
where

import Elea.Prelude
import Elea.Type hiding ( empty )
import Elea.Monad.Parser.Class ( DefinedTerm )
import qualified Data.Map as Map

data Database 
  = Database    { dbTerms :: Map String DefinedTerm
                , dbInds :: Map String (Poly Ind) }
                
empty :: Database
empty = Database Map.empty Map.empty
                
lookupTerm :: String -> Database -> Maybe DefinedTerm
lookupTerm n = Map.lookup n . dbTerms

lookupInd :: String -> Database -> Maybe (Poly Ind)
lookupInd n = Map.lookup n . dbInds

defineTerm :: String -> DefinedTerm -> Database -> Database
defineTerm n t db = db { dbTerms = Map.insert n t (dbTerms db) }

defineInd :: String -> Poly Ind -> Database -> Database
defineInd n i db = db { dbInds = Map.insert n i (dbInds db) }

