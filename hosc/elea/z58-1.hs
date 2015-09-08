data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;
data Pair a b = Pair a b;

drop n (zip xs ys)  where

drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
  };
  
zip = \xs ys ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case ys of {
        Nil -> Nil;
        Cons y ys1 -> Cons (Pair x y) (zip xs1 ys1);
      };
  };