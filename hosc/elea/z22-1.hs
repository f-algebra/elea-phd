data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

max (max n m) k where

max = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> case m of {
      Z -> Suc n1;
      Suc m1 -> Suc (max n1 m1);
    };
  };