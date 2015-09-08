data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

drop n (map f xs)  where

drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
  };
  
map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> Cons (f x) (map f xs1);
  };