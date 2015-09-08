data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;
data Pair a b = Pair a b;

Nil where

zip = \xs ys ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case ys of {
        Nil -> Nil;
        Cons y ys1 -> Cons (Pair x y) (zip xs1 ys1);
      };
  };