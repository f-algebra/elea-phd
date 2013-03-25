-- | de-Bruijn indices, lifting, substitution, and unification.
module Elea.Index
(
  Index, Liftable (..), Substitutable (..),
  Unifiable (..), Unifier, unifierUnion, unify,
  lift, liftMany, liftManyAt, subst, 
  lowerAt, lower, lowerMany, replaceAt,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import qualified Data.Map as Map
import qualified Elea.Monad.Failure as Fail

-- | A de-Bruijn index.
newtype Index 
  = Index Nat
  deriving ( Eq, Ord, Enum, Num )
  
class Liftable a where
  liftAt :: Index -> a -> a
  
lift :: Liftable a => a -> a
lift = liftAt 0

liftMany :: Liftable a => Int -> a -> a
liftMany n = concatEndos (replicate n lift)

liftManyAt :: Liftable a => Int -> Index -> a -> a
liftManyAt n at = concatEndos (replicate n (liftAt at))
   
instance Liftable () where
  liftAt _ = id
  
instance Liftable Index where
  liftAt at x 
    | at <= x = succ x
    | otherwise = x
    
instance (Liftable a, Liftable b) => Liftable (a, b) where
  liftAt at (x, y) = (liftAt at x, liftAt at y)

instance (Liftable a, Liftable b) => Liftable (Either a b) where
  liftAt at (Left x) = Left (liftAt at x)
  liftAt at (Right y) = Right (liftAt at y)

instance Show Index where
  show (Index n) = "_" ++ show n
  
class Liftable t => Substitutable t where
  substAt :: Index -> t -> t -> t
  freeIndices :: t -> Set Index
 
  -- | For debugging purposes only.
  failure :: t
  
subst :: Substitutable t => t -> t -> t
subst = substAt 0

-- | Performs substitution without the accompanying lowering of indices.
replaceAt :: Substitutable t => Index -> t -> t -> t
replaceAt at with = substAt at with . liftAt (succ at)

lowerAt :: Substitutable t => Index -> t -> t
lowerAt idx = substAt idx failure --(error "Lowered an existing index")

lower :: Substitutable t => t -> t
lower = lowerAt 0

lowerMany :: Substitutable t => Int -> t -> t
lowerMany n = concatEndos (replicate n lower)

-- | Applies the given unifier to the given term.
unify :: Substitutable t => Unifier t -> t -> t
unify uni_map = 
  flip (foldr (uncurry substAt)) (Map.toAscList uni_map)

-- | Fails if the two unifiers have conflicting substitutions.
unifierUnion :: (Eq t, Fail.Monad m) => Unifier t -> Unifier t -> m (Unifier t)
unifierUnion uni1 uni2 = do
  Fail.unless all_eq
  return (uni1 `Map.union` uni2)
  where
  -- Returns whether all of the elements subsituted for equal indices
  -- are equal elements. If so then you can take the union of the 
  -- two substitution maps safely.
  all_eq = and 
    . Map.elems
    $ Map.intersectionWith (==) uni1 uni2
 
type Unifier a = Map Index a

class Substitutable t => Unifiable t where
  -- | Returns the unifier that should be applied to the first argument
  -- to produce the second, fails if no such unifier exists. 
  unifier :: Fail.Monad m => t -> t -> m (Unifier t)

