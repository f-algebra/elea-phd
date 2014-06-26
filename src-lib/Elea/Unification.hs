module Elea.Unification 
(
  Unifier, Unifiable (..), 
  union, unions, singleton, exists
)
where

import Elea.Prelude hiding ( union, find )
import Elea.Index
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Map as Map
import qualified Data.Set as Set

type Unifier a = Map Index a

class (Eq (Inner t), Substitutable t) => Unifiable t where
  -- | Returns the unifier that should be applied to the first argument
  -- to produce the second, fails if no such unifier exists. 
  find :: Fail.Can m => t -> t -> m (Unifier (Inner t))
  
  -- | Apply a unifier, so @apply (find t1 t2) t1 ==> t2@.
  apply :: Unifier (Inner t) -> t -> t
  
  -- | A special comparison function for which
  -- inequality implies non-unifiability.
  -- We can use this for fast lookup of potentially unifiable terms, in the
  -- same way that a hash allows us to do fast lookup, since 
  -- inequality of hashes implies inequality of orignals.
  gcompare :: t -> t -> Ordering
  

singleton :: Index -> a -> Unifier a
singleton = Map.singleton

-- | Fails if the two unifiers have conflicting substitutions.
union :: (Eq t, Fail.Can m) => Unifier t -> Unifier t -> m (Unifier t)
union uni1 uni2 = do
  Fail.unless all_eq
  return (uni1 `Map.union` uni2)
  where
  -- Returns whether all of the elements subsituted for equal indices
  -- are equal elements. If so then you can take the union of the 
  -- two substitution maps safely.
  all_eq = and 
    . Map.elems
    $ Map.intersectionWith (==) uni1 uni2
    
unions :: (Eq t, Fail.Can m) => [Unifier t] -> m (Unifier t)
unions = foldrM union mempty 

exists :: Unifiable t => t -> t -> Bool
exists t = isJust . find t


instance Unifiable t => Unifiable [t] where
  find xs ys = do
    Fail.unless ((length xs :: Int) == length ys)
    unis <- zipWithM find xs ys
    unions unis
    
  apply uni = 
    map (apply uni)
    
  gcompare xs = 
    mconcat . zipWith gcompare xs

    
instance (Ord t, Unifiable t) => Unifiable (Set t) where
  find xs ys = 
    find (Set.toAscList xs) (Set.toAscList ys)
    
  apply uni = 
    Set.map (apply uni)
    
  gcompare xs ys = 
    mconcat $ zipWith gcompare (toList xs) (toList ys)

