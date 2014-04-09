-- | A dictionary with unifiable objects as keys, where lookup will find any 
-- keys unifiable with the one provided, returning both the stored value and
-- the unifier.
-- Requires that the key be 'Generalisable' for fast lookup.
module Elea.Unification.Map
(
  Generalised, Generalisable (..),
  
  UMap,
  empty, lookup, insert,
)
where

import Prelude ()
import Elea.Prelude hiding ( find, lookup )
import Elea.Index
import Elea.Unification
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set
import qualified Data.Map as Map


newtype Generalised a = Generalised a
  deriving ( Show, Functor )
  
instance Applicative Generalised where
  pure = Generalised
  Generalised f <*> Generalised a = Generalised (f a)

-- | A generalised object is one for which inequality implies non-unifiability. 
-- Think of it like a unifiability hash, in that inequality of hashed values
-- implies inequality of original values.
-- We use this for fast lookup of potentially unifiable terms in 'UMap', in the
-- same way as a hash gives fast lookup in a hash-map - by screening out
-- non-equal, and hence non-unifiable, variables.
class Unifiable a => Generalisable a where
  -- | Implement this by running some form of generalisation, such that 
  -- 'compareGen' will return 'EQ' for potentially unifiable objects, 
  -- then use 'pure' wrap the value in 'Generalised'.
  generalise :: a -> Generalised a
  
  -- | The special comparison function for which 
  -- inequality implies non-unifiability,
  -- provided the first argument has gone through 'generalise'.
  compareGen :: a -> a -> Ordering
  
  
-- | The instance of 'Eq' induced by 'compareGen'
instance Generalisable a => Eq (Generalised a) where
  Generalised x == Generalised y = compareGen x y == EQ
  
-- | The instance of 'Ord' induced by 'compareGen'
instance Generalisable a => Ord (Generalised a) where
  Generalised x `compare` Generalised y = compareGen x y

  
newtype UMap k a = UMap { getMap :: Map (Generalised k) [(k, a)] }
  deriving ( Show )
  
empty :: UMap k a
empty = UMap Map.empty

insert :: forall k a . (Eq k, Generalisable k) 
  => k -> a -> UMap k a -> UMap k a
insert k a = UMap . Map.alter ins (generalise k) . getMap
  where
  ins :: Maybe [(k, a)] -> Maybe [(k, a)]
  ins Nothing = Just [(k, a)]
  ins (Just ks) = Just ((k, a) : ks')
    where
    ks' = filter ((/= k) . fst) ks
  
lookup :: forall m k a . (Fail.Can m, Generalisable k) 
  => k -> UMap k a -> m (Unifier (Inner k), a)
lookup k (UMap dmap) = do
  kas <- Fail.mapLookup (pure k) dmap
  Fail.choose (map findUni kas)
  where
  findUni :: (k, a) -> m (Unifier (Inner k), a)
  findUni (k', a) = do
    uni <- find k' k
    return (uni, a)
    

