data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

drop (add m n) xs  where

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
  