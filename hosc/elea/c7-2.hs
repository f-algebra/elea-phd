data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

bottom where

drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
  }; 
  
repeat = \x -> Cons x (repeat x);
iterate = \f x -> Cons x (iterate f (f x));

map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> Cons (f x) (map f xs1);
  };
  
filter = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> Cons x (filter p xs1);
        False -> filter p xs1;
      };
  };
  
butlast = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> 
      case xs1 of {
        Nil -> Nil;
        Cons x2 xs2 ->
          Cons x2 (butlast xs2);
      };
  };
  
last = \xs -> 
  case xs of {
    Nil -> Z;
    Cons x1 xs1 ->
      case xs1 of {
        Nil -> x1;
        Cons x2 xs2 ->
          last xs1;
      };
  };
  
tail = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> xs1;
  };
 
bottom = bottom;
 