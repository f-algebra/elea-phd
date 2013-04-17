-- | de-Bruijn indices, lifting, substitution, and unification.
module Elea.Index
(
  Index, Liftable (..), Substitutable (..), Failure (..),
  lift, liftMany, liftManyAt, subst, 
  lowerAt, lower, lowerMany, replaceAt, omega,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import qualified Data.Nat as Nat
import qualified Data.Map as Map
import qualified Elea.Monad.Failure as Fail

-- | A de-Bruijn index.
-- We use the natural numbers with omega, in order to leverage some tricks 
-- using omega as a temporary index. The omega index is greater than every
-- other, has decidable equality, and is unaffected by lifting/lowering.
newtype Index 
  = Index CoNat
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
  
-- | This is for debugging. It is used in places that should really be 
-- undefined, but it's nicer to use a value we can actually observe/print out.
class Failure t where
  failure :: t
  
class Liftable t => Substitutable t where
  type Inner t
  substAt :: Index -> Inner t -> t -> t
  free :: t -> Set Index
  
subst :: Substitutable t => Inner t -> t -> t
subst = substAt 0

-- | Performs substitution without the accompanying lowering of indices.
replaceAt :: Substitutable t => Index -> Inner t -> t -> t
replaceAt at with = substAt at with . liftAt (succ at)

lowerAt :: (Substitutable t, Failure (Inner t)) => Index -> t -> t
lowerAt idx = substAt idx failure --(error "Lowered an existing index")

lower :: (Substitutable t, Failure (Inner t)) => t -> t
lower = lowerAt 0

lowerMany :: (Substitutable t, Failure (Inner t)) => Int -> t -> t
lowerMany n = concatEndos (replicate n lower)

-- The magic index. Equal only to itself, greater than every other index,
-- and unchanged by lifting or lowering.
omega :: Index
omega = Index Nat.omega

