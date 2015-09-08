data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

takeWhile (\x -> True) xs where

takeWhile = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> Cons x (takeWhile p xs1);
        False -> Nil;
      };
  };
