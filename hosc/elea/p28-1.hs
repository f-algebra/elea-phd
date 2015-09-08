data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

revflat xs where

app = \xs ys ->
	case xs of {
		Nil -> ys;
		Cons x1 xs1 -> Cons x1 (app xs1 ys);
	};

revflat = \xss ->
  case xss of {
    Nil -> Nil;
    Cons xs1 xss1 ->
      app (revflat xss) xs1;
  };
  
itrevflat = \xss yss ->
  case xss of {
    Nil -> Nil;
    Cons xs1 xss1 ->
      itrevflat xss1 (app xs1 yss);
  };