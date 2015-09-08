data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

filter p (app xs ys) where

filter = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> Cons x (filter p xs1);
        False -> filter p xs1;
      };
  };
  
app = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (app xs1 ys);
	};
	