data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

drop (Suc n) (Cons x xs) where

drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
  };
  