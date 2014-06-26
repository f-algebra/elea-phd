-- | A dictionary with unifiable objects as keys, where lookup will find any 
-- keys unifiable with the one provided, returning both the stored value and
-- the unifier.
-- Uses the 'gcompare' function for fast lookup.
module Elea.Unification.Map
(
  UMap,
  empty, 
  singleton,
  lookup, 
  insert,
  toList,
  elems
)
where

import Elea.Prelude hiding ( find, lookup, toList )
import Elea.Index
import Elea.Unification hiding ( singleton, toList )
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set
import qualified Data.Map as Map


newtype Generalised a = Generalised a
  deriving ( Show, Functor )
  
instance Applicative Generalised where
  pure = Generalised
  Generalised f <*> Generalised a = Generalised (f a)
  
  
-- | The instance of 'Eq' induced by 'gcompare'
instance Unifiable a => Eq (Generalised a) where
  Generalised x == Generalised y = gcompare x y == EQ
  
-- | The instance of 'Ord' induced by 'compareGen'
instance Unifiable a => Ord (Generalised a) where
  Generalised x `compare` Generalised y = gcompare x y

  
newtype UMap k a = UMap { getMap :: Map (Generalised k) [(k, a)] }
  deriving ( Show )
  
empty :: UMap k a
empty = UMap Map.empty

singleton :: forall k a . (Ord k, Unifiable k) => k -> a -> UMap k a
singleton k a = insert k a empty

-- | The insertion operation favours the more general key w.r.t unification.
-- So if @k1 -> a1@ is inside the map, and we insert @k2 -> a2@, then:
--   1. If @Unifier.exists k1 k2@ then we don't insert @k2 -> a2@, since @k1@ 
--      is the more general.
--   2. If @Unifier.exists k2 k1@ then we remove @k1 -> a1@ 
--      and insert @k2 -> a2@.
--   3. Otherwise we just insert @k2 -> a2@ and keep @k1 -> a1@.
insert :: forall k a . (Ord k, Unifiable k) 
  => k -> a -> UMap k a -> UMap k a
insert k a = UMap . Map.alter ins (pure k) . getMap
  where
  ins :: Maybe [(k, a)] -> Maybe [(k, a)]
  ins Nothing = Just [(k, a)]
  ins (Just ks) 
    | any ((`subsumes` k) . fst) ks = Just ks
    | otherwise = Just ((k, a) : filter ((k `subsumes`) . fst) ks)
    where
    subsumes :: k -> k -> Bool
    subsumes = exists
  
lookup :: forall m k a . (Fail.Can m, Unifiable k) 
  => k -> UMap k a -> m (Unifier (Inner k), a)
lookup k (UMap dmap) = do
  kas <- Fail.mapLookup (pure k) dmap
  Fail.choose (map findUni kas)
  where
  findUni :: (k, a) -> m (Unifier (Inner k), a)
  findUni (k', a) = do
    uni <- find k' k
    return (uni, a)
    
toList :: UMap k a -> [(k, a)]
toList = concat . Map.elems . getMap

elems :: UMap k a -> [a]
elems = map snd . toList
    
instance (Ord k, Unifiable k) => Monoid (UMap k a) where
  mempty = empty
  mappend map1 = foldr (uncurry insert) map1 . toList
