data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

take n (map f xs) where

take = \n xs ->
  case n of {
    Z -> Nil;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> Cons x (take n1 xs1);
      };
  };
  
map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> Cons (f x) (map f xs1);
  };
