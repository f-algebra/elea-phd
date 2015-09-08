data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

mul n m where

add = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> Suc (add n1 m);
  };

mul = \n m ->
  case n of {
    Z -> Z;
    Suc n1 ->
      add m (mul n1 m);
  };

itmul = \n m k ->
  case n of {
    Z -> k;
    Suc n1 ->
      itmul n1 m (add m k);
  };