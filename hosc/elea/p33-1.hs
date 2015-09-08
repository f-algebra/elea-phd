data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

fac n where

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

fac = \n -> 
  case n of {
    Z -> Suc Z;
    Suc n1 -> mul n (fac n1);
  };
  
itfac = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> itfac n1 (mul n m);
  };