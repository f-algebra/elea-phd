-- | de-Bruijn indices, lifting, and substitution.
module Elea.Index
(
  Index, Indexed (..), Substitutable (..),
  lift, liftAt, liftManyAt, liftMany, subst, 
  lowerAt, lower, lowerMany, lowerableBy,
  replaceAt, tryLowerMany,
  omega, containsOmega,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import qualified Data.Nat as Nat
import qualified Data.Map as Map
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set

-- | A de-Bruijn index.
-- We use the natural numbers with omega, in order to leverage some tricks 
-- using omega as a temporary index. The omega index is greater than every
-- other, has decidable equality, and is unaffected by lifting/lowering.
newtype Index 
  = Index CoNat
  deriving ( Eq, Ord, Enum, Num )
  
-- | Indexed things contain de-Bruijn indices.
class Indexed t where
  free :: t -> Set Index
  
  -- | Only modifies free indices.
  shift :: (Index -> Index) -> t -> t
  
  freeWithin :: Index -> t -> Bool
  freeWithin x = Set.member x . free
  

liftManyAt :: Indexed a => Nat -> Index -> a -> a
liftManyAt n at = shift lift
  where
  lift x
    | at <= x = enum n + x
    | otherwise = x

liftAt :: Indexed a => Index -> a -> a
liftAt = liftManyAt 1

lift :: Indexed a => a -> a
lift = liftAt 0

liftMany :: Indexed a => Nat -> a -> a
liftMany = flip liftManyAt 0

lowerManyAt :: Indexed a => Nat -> Index -> a -> a
lowerManyAt n at = shift lower
  where
  lower x
    | at <= x = x - enum n
    | otherwise = x
    
lowerAt :: Indexed a => Index -> a -> a
lowerAt = lowerManyAt 1

lower :: Indexed a => a -> a
lower = lowerAt 0

lowerMany :: Indexed a => Nat -> a -> a
lowerMany = flip lowerManyAt 0

instance Indexed () where
  free _ = mempty
  shift _ = id
  
instance Indexed Index where
  free = Set.singleton
  shift = ($)
    
instance (Indexed a, Indexed b) => Indexed (a, b) where
  free (x, y) = free x ++ free y
  shift f (x, y) = (shift f x, shift f y)

instance (Indexed a, Indexed b) => Indexed (Either a b) where
  free (Left x) = free x
  free (Right y) = free y
  
  shift f (Left x) = Left (shift f x)
  shift f (Right y) = Right (shift f y)

instance (Ord a, Indexed a) => Indexed (Set a) where
  free = Set.unions . map free . Set.toList
  shift f = Set.map (shift f)
  
instance Indexed a => Indexed [a] where
  free = Set.unions . map free
  shift f = map (shift f)
  
instance Show Index where
  show (Index n) = "_" ++ show n

class Indexed t => Substitutable t where
  type Inner t
  substAt :: Index -> Inner t -> t -> t
  
subst :: Substitutable t => Inner t -> t -> t
subst = substAt 0

-- | Performs substitution without the accompanying lowering of indices.
replaceAt :: Substitutable t => Index -> Inner t -> t -> t
replaceAt at with = substAt at with . liftAt (succ at)

lowerableBy :: Indexed t => Nat -> t -> Bool
lowerableBy n = all (>= enum n) . free

tryLowerMany :: Indexed t => Nat -> t -> Maybe t
tryLowerMany n t = do
  guard (lowerableBy n t)
  return (lowerMany n t)

-- | The magic index. Equal only to itself, greater than every other index,
-- and unchanged by lifting or lowering.
-- Used in Elea.Context as the gap variable.
omega :: Index
omega = Index Nat.omega

-- | Check whether 'omega' is free within a term
containsOmega :: Indexed t => t -> Bool
containsOmega = freeWithin omega

