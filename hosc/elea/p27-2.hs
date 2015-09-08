data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

itrev xs Nil where

app = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (app xs1 ys);
	};

rev = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 ->
      app (rev xs) (Cons x1 Nil);
  };
  
itrev = \xs ys ->
  case xs of {
    Nil -> ys;
    Cons x1 xs1 ->
      itrev xs1 (Cons x1 ys);
  };