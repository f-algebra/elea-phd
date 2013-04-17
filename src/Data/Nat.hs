module Data.Nat 
(
  Nat, CoNat, omega
)
where

-- | Natural numbers. Use 'toEnum' to construct.
newtype Nat = Nat Int
  deriving ( Eq, Ord )

-- | Co-inductive natural numbers with decidable equality (magic).
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
    | x > y = Nat (x - y)
    | otherwise = Nat 0
  Nat x * Nat y = Nat (x * y)
  abs = id
  negate = undefined
  signum = undefined
  fromInteger n | n >= 0 = Nat (fromInteger n)
  
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
  
  CoNat _ - Omega = undefined
  Omega - CoNat _ = Omega
  CoNat x - CoNat y = CoNat (x - y)
  
  Omega * _ = Omega
  _ * Omega = Omega
  CoNat x * CoNat y = CoNat (x * y)
  abs = id
  negate = undefined
  signum = undefined
  fromInteger n = CoNat (fromInteger n)

omega :: CoNat
omega = Omega
