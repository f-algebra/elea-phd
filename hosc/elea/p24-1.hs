data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

even (add n m)  where

even = \n -> 
  case n of {
    Z -> True;
    Suc n1 ->
      case n1 of {
        Z -> False;
        Suc n2 -> even n2;
      };
  };
  
add = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> Suc (add n1 m);
  };
