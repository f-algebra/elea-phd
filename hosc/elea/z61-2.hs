data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

lastOfTwo xs ys where

lastOfTwo = \xs ys ->
  case ys of {
    Nil -> last xs;
    Cons y1 ys1 -> last ys;
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
  
