data Bool = True | False;
data List a = Nil | Cons a (List a);
data Nat = Z | Suc Nat;

dropWhile (\x -> False) xs where

dropWhile = \p xs ->
  case xs of {
    Nil -> Nil;
    Cons x xs1 -> 
      case p x of {
        True -> dropWhile p xs1;
        False -> Cons x xs1;
      };
  };
  