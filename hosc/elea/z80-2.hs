data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

take n (app xs ys) where

len = \xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 -> Suc (len xs1);
  };
  
app = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (app xs1 ys);
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