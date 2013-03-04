-- | de-Bruijn indices and lifting.
module Elea.Index
(
  Index, Liftable (..), Substitutable (..),
  lift, subst, lowerAt, lower, lowerMany,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )

newtype Index = Index Nat
  deriving ( Eq, Ord, Enum, Num )
  
class Liftable a where
  liftAt :: Index -> a -> a
  
lift :: Liftable a => a -> a
lift = liftAt 0
   
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
  
class Substitutable t where
  substAt :: Index -> t -> t -> t

subst :: Substitutable t => t -> t -> t
subst = substAt 0
  
lowerAt :: Substitutable t => Index -> t -> t
lowerAt idx = substAt idx undefined

lower :: Substitutable t => t -> t
lower = lowerAt 0

lowerMany :: Substitutable t => Int -> t -> t
lowerMany n = concatEndos (replicate n lower)

