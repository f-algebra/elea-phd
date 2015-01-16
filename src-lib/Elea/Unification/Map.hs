-- | A dictionary with unifiable objects as keys, where lookup will find any 
-- keys unifiable with the one provided, returning both the stored value and
-- the unifier.
-- Uses the 'gcompare' function for fast lookup.
module Elea.Unification.Map
(
  UMap,
  Generalised (..),
  empty, 
  singleton,
  lookup, 
  insert,
  toList,
  elems,
  lookupLG,
  lookupAlphaEq,
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
  
-- | The instance of 'Ord' induced by 'gcompare'
instance Unifiable a => Ord (Generalised a) where
  Generalised x `compare` Generalised y = gcompare x y

  
newtype UMap k a = UMap { getMap :: Map (Generalised k) [(k, a)] }
  deriving ( Show )
  
empty :: UMap k a
empty = UMap Map.empty

singleton :: forall k a . (Ord k, Unifiable k) => k -> a -> UMap k a
singleton k a = insert k a empty

insert :: forall k a . (Ord k, Unifiable k) 
  => k -> a -> UMap k a -> UMap k a
insert k a = UMap . Map.alter ins (pure k) . getMap
  where
  ins :: Maybe [(k, a)] -> Maybe [(k, a)]
  ins Nothing = Just [(k, a)]
  ins (Just ks) = Just ((k, a) : filter (not . alphaEq k . fst) ks)
  
  {-
  ins (Just ks) 
    | any ((`subsumes` k) . fst) ks = Just ks
    | otherwise = Just ((k, a) : filter ((k `subsumes`) . fst) ks)
    where
    subsumes :: k -> k -> Bool
    subsumes = exists
    -}
  
lookup :: forall m k a . (Ord (Inner k), Unifiable k)
  => k -> UMap k a -> [(k, (Unifier (Inner k), a))]
lookup k (UMap dmap) = 
  case Map.lookup (pure k) dmap of
    Nothing -> []
    Just kas -> mapMaybe findUni kas
  where
  findUni :: (k, a) -> Maybe (k, (Unifier (Inner k), a))
  findUni (k', a) = do
    uni <- find k' k
    return (k', (uni, a))

-- | Lookup a least general unifier. Not guaranteed to be unique.
lookupLG :: (Fail.Can m, Ord (Inner k), Unifiable k) 
  => k -> UMap k a -> m (Unifier (Inner k), a)
lookupLG k map = do
  Fail.when (null lgs)
  return (snd (head lgs))
  where
  lgs = leastGeneral (lookup k map)
  
  
lookupAlphaEq :: (Fail.Can m, Ord (Inner k), Unifiable k) 
  => k -> UMap k a -> m (Unifier (Inner k), a)
lookupAlphaEq k map = do
  Fail.when (null kuas)
  return (snd (head kuas))
  where
  kuas = id
    . filter (\(k', _) -> alphaEq k k')
    $ lookup k map
  
                             
toList :: UMap k a -> [(k, a)]
toList = concat . Map.elems . getMap

elems :: UMap k a -> [a]
elems = map snd . toList
    
instance (Ord k, Unifiable k) => Monoid (UMap k a) where
  mempty = empty
  mappend map1 = foldr (uncurry insert) map1 . toList
