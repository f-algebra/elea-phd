data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

min (min n m) k where

min = \n m ->
  case n of {
    Z -> Z;
    Suc n1 -> case m of {
      Z -> Z;
      Suc m1 -> Suc (min n1 m1);
    };
  };
  