data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

take (Suc n) (Cons x xs) where

take = \n xs ->
  case n of {
    Z -> Nil;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> Cons x (take n1 xs1);
      };
  };
  