data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

itexp x n (Suc Z) where

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

exp = \x n -> 
  case n of {
    Z -> Suc Z;
    Suc n1 -> mul x (exp x n1);
  };
  
itexp = \x n acc ->
  case n of {
    Z -> acc;
    Suc n1 -> itexp x n1 (mul x acc);
  };