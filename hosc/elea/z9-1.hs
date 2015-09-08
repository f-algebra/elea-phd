data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

minus (minus n m) k  where

add = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> Suc (add n1 m);
  };
  
minus = \n m ->
  case m of {
    Z -> case n of {
      Z -> Z;
      Suc n1 -> Suc n1;
    };
    Suc m1 -> case n of {
      Z -> Z;
      Suc n1 -> minus n1 m1;
    };
  };