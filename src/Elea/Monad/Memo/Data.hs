-- | A memoiser for steps
module Elea.Monad.Memo.Data
(
  Data,
  lookup,
  insert,
)
where

import Prelude ()
import Elea.Prelude hiding ( lookup )
import Elea.Term
import Elea.Unification ( Unifier )
import Elea.Monad.Env ()
import Elea.Transform.Names ( Name )
import qualified Elea.Term.Ext as Term
import qualified Elea.Type as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Unification.Map as UMap
import qualified Elea.Monad.Failure.Class as Fail

import qualified Data.IntMap as Map
import qualified Data.Set as Set

newtype Data
  = Data (IntMap (UMap.UMap Term (Maybe Term)))
  deriving ( Show )

mkLabels [ ''Data ]

instance Empty Data where
  empty = Data Map.empty
  
-- | Failure means there is no entry. Returning Nothing means
-- that this step failed and this failure was memoised.
lookup :: Fail.Can m => Name -> Term -> Data -> m (Maybe Term)
lookup (enum -> name) term (Data map) = 
  case Map.lookup name map of
    Nothing -> Fail.here
    Just umap -> do
      (uni, mby_t) <- UMap.lookupAlphaEq term umap
      return (fmap (Unifier.apply uni) mby_t)

insert :: Name -> Term -> Maybe Term -> Data -> Data
-- insert _ term _ db
 -- | (not . Set.null . Indices.free) term = db
insert (enum -> name) term outcome (Data map) = 
  Data (Map.alter ins name map)
  where
  ins Nothing = Just (UMap.singleton term outcome)
  ins (Just umap) = Just (UMap.insert term outcome umap)
  
