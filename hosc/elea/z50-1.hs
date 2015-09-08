data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;
data Pair a b = Pair a b;

butlast xs where

butlast = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> 
      case xs1 of {
        Nil -> Nil;
        Cons x2 xs2 ->
          butlast (Cons x2 xs2);
      };
  };
  
take = \n xs ->
  case n of {
    Z -> Nil;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> Cons x (take n1 xs1);
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
  

len = \xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 -> Suc (len xs1);
  };