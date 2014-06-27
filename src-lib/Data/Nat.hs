module Data.Nat 
(
  Nat, CoNat, omega
)
where

import Prelude

-- | Natural numbers. Use 'toEnum' to construct.
newtype Nat = Nat Int
  deriving ( Eq, Ord )

-- | Co-inductive natural numbers with decidable equality.
data CoNat = Omega | CoNat Nat
  deriving ( Eq )
  
instance Ord CoNat where
  Omega `compare` Omega = EQ
  Omega `compare` _ = GT
  _ `compare` Omega = LT
  CoNat x `compare` CoNat y = x `compare` y
  
instance Show Nat where
  show (Nat x) = show x
  
instance Enum Nat where
  succ (Nat n) = Nat (n + 1)
  pred (Nat n) | n > 0 = Nat (n - 1)
  toEnum n | n >= 0 = Nat n
  fromEnum (Nat n) = n
  
instance Num Nat where
  Nat x + Nat y = Nat (x + y)
  Nat x - Nat y 
    | x >= y = Nat (x - y)
    | otherwise = 
      error $ "Cannot subtract " ++ show y ++ " from " ++ show x ++ "."
  Nat x * Nat y = Nat (x * y)
  abs = id
  negate = error "Cannot negate a natural number"
  signum (Nat x) = Nat (signum x)
  fromInteger n 
    | n >= 0 = Nat (fromInteger n)
    | otherwise = error (show n ++ " is not a natural number.")
  
instance Show CoNat where
  show Omega = "inf"
  show (CoNat n) = show n

instance Enum CoNat where
  succ Omega = Omega
  succ (CoNat n) = CoNat (succ n)
  
  pred Omega = Omega
  pred (CoNat n) = CoNat (pred n)
  
  toEnum n = CoNat (toEnum n)
  fromEnum (CoNat n) = fromEnum n
  
instance Num CoNat where
  Omega + _ = Omega
  _ + Omega = Omega
  CoNat x + CoNat y = CoNat (x + y)
  
  CoNat x - CoNat y = CoNat (x - y)
  Omega - CoNat _ = Omega
  CoNat _ - Omega = CoNat 0
  
  Omega * Omega = Omega
  Omega * CoNat x 
    | x == 0 = CoNat 0
    | otherwise = Omega
  x * Omega = Omega * x
  CoNat x * CoNat y = CoNat (x * y)
  
  signum (CoNat x) = CoNat (signum x)
  signum Omega = CoNat 1
  
  abs = id
  negate = error "Cannot negate a co-natural number"
  fromInteger = CoNat . fromInteger

omega :: CoNat
omega = Omega
