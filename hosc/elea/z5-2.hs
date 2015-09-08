data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

implies (eq n m) (count n (Cons m xs)) where

eq = \n m ->
  case n of {
    Z -> case m of {
      Z -> True;
      Suc m1 -> False; 
    };
    Suc n1 -> case m of {
      Z -> False;
      Suc m1 -> eq n1 m1;
    };
  };
  
count = \n xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 ->
      case eq n x of {
        True -> Suc (count n xs1);
        False -> count n xs1;
      };
  };
  
implies = \p q -> 
  case p of {
    True -> q;
    False -> Z; 
  };