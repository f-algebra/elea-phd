module Data.Nat 
(
  Nat
)
where

-- | Natural numbers.
newtype Nat = Nat Int
  deriving ( Eq, Ord )
  
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

