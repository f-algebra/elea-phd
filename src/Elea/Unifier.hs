module Elea.Unifier 
(
  Unifier, Unifiable (..), 
  apply, union, unions, singleton
)
where

import Prelude ()
import Elea.Prelude hiding ( union )
import Elea.Index
import qualified Elea.Monad.Failure as Fail
import qualified Data.Map as Map

type Unifier a = Map Index a

apply :: Substitutable t => Unifier (Inner t) -> t -> t
apply = flip (foldr (uncurry substAt)) . Map.toAscList

singleton :: Index -> a -> Unifier a
singleton = Map.singleton

-- | Fails if the two unifiers have conflicting substitutions.
union :: (Eq t, Fail.Monad m) => Unifier t -> Unifier t -> m (Unifier t)
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
    
unions :: (Eq t, Fail.Monad m) => [Unifier t] -> m (Unifier t)
unions = foldrM union mempty 

class Substitutable t => Unifiable t where
  -- | Returns the unifier that should be applied to the first argument
  -- to produce the second, fails if no such unifier exists. 
  find :: Fail.Monad m => t -> t -> m (Unifier (Inner t))

