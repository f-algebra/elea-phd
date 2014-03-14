-- | de-Bruijn indices, lifting, and substitution.
module Elea.Index
(
  Index, 
  Indexed (..), 
  Substitutable (..),
  Shift,
  lift, liftAt, liftManyAt, liftMany, subst, 
  lowerAt, lower, lowerMany, lowerableBy,
  freeWithin, replaceAt, tryLowerMany,
  omega, containsOmega,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift, replaceAt )
import qualified Data.Nat as Nat
import qualified Data.Map as Map
import qualified Elea.Monad.Failure.Class as Fail
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
  
  
{-# RULES
  "shift natural" 
    forall f g xs . shift f (shift g xs) = shift (f . g) xs ;
    
  "liftMany compose" 
    forall n m xs . liftMany n (liftMany m xs) = liftMany (n + m) xs ;
    
  "lowerMany compose"
    forall n m xs . lowerMany n (lowerMany m xs) = lowerMany (n + m) xs ;
  #-}
  
  
-- | Functions which shift indices. A useful synonym.
type Shift = forall a . Indexed a => a -> a
  
freeWithin :: Indexed a => Index -> a -> Bool
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
  
instance Substitutable t => Substitutable [t] where
  type Inner [t] = Inner t
  substAt i x = map (substAt i x)
  
instance (Ord t, Substitutable t) => Substitutable (Set t) where
  type Inner (Set t) = Inner t
  substAt i x = Set.map (substAt i x)
  
subst :: Substitutable t => Inner t -> t -> t
subst = substAt 0

-- | Performs substitution without the accompanying lowering of indices.
replaceAt :: Substitutable t => Index -> Inner t -> t -> t
replaceAt at with = substAt at with . liftAt (succ at)

lowerableBy :: Indexed t => Nat -> t -> Bool
lowerableBy n = all (>= enum n) . free

tryLowerMany :: (Fail.Can m, Indexed t) => Nat -> t -> m t
tryLowerMany n t = do
  Fail.unless (lowerableBy n t)
  return (lowerMany n t)

-- | The magic index. Equal only to itself, greater than every other index,
-- and unchanged by lifting or lowering.
-- Used in Elea.Context as the gap variable.
omega :: Index
omega = Index Nat.omega

-- | Check whether 'omega' is free within a term
containsOmega :: Indexed t => t -> Bool
containsOmega = freeWithin omega

