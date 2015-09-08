data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

len (drop n xs) where

len = \xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 -> Suc (len xs1);
  };
  
drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
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