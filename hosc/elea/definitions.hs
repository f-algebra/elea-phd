data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;
data Pair a b = Pair a b;

repeat = \x -> Cons x (repeat x);
iterate = \f x -> Cons x (iterate f (f x));

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
  
drop = \n xs ->
  case n of {
    Z -> xs;
    Suc n1 -> 
      case xs of {
        Nil -> Nil;
        Cons x xs1 -> drop n1 xs1;
      };
  };
  
add = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> Suc (add n1 m);
  };
  
mul = \n m ->
  case n of {
    Z -> Z;
    Suc n1 ->
      add m (mul n1 m);
  };
  
max = \n m ->
  case n of {
    Z -> m;
    Suc n1 -> case m of {
      Z -> Suc n1;
      Suc m1 -> Suc (max n1 m1);
    };
  };
  
min = \n m ->
  case n of {
    Z -> Z;
    Suc n1 -> case m of {
      Z -> Z;
      Suc m1 -> Suc (min n1 m1);
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
  
map = \f xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> Cons (f x) (map f xs1);
  };
  
filter = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> Cons x (filter p xs1);
        False -> filter p xs1;
      };
  };
  
compose = \f g x -> f (g x);

tail = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> xs1;
  };

len = \xs ->
  case xs of {
    Nil -> Z;
    Cons x xs1 -> Suc (len xs1);
  };
  
dropWhile = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> dropWhile p xs1;
        False -> Cons x xs1;
      };
  };
  
takeWhile = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> Cons x (takeWhile p xs1);
        False -> Nil;
      };
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

butlast = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 -> 
      case xs1 of {
        Nil -> Nil;
        Cons x2 xs2 ->
          Cons x2 (butlast xs2);
      };
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
 
rev = \xs ->
  case xs of {
    Nil -> Nil;
    Cons x1 xs1 ->
      app (rev xs) (Cons x1 Nil);
  };
  
it_rev = \xs ys ->
  case xs of {
    Nil -> ys;
    Cons x1 xs1 ->
      it_rev xs1 (Cons x1 ys);
  };
  
even = \n -> 
  case n of {
    Z -> True;
    Suc n1 ->
      case n1 of {
        Z -> False;
        Suc n2 -> even n2;
      };
  };
