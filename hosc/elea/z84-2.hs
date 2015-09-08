data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;
data Pair a b = Pair a b;

zip xs (app ys zs) where

take = \n xs ->
  case n of {
    Z -> Nil;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> Cons x (take n1 xs1);
      };
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
  
app = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (app xs1 ys);
	};
	
len = \xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 -> Suc (len xs1);
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