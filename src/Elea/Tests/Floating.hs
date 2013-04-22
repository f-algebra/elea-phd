module Elea.Tests.Floating
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )
import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp

assertFloatEq :: String -> String -> Elea Test.Test
assertFloatEq aim_s t_s = do
  t <- Test.term t_s
  aim <- Test.term aim_s
  return (Test.assertEq (Float.run aim) (Float.run t))

tests = Test.label "Floating"
    $ Test.run $ do
  Test.loadPrelude
  
  
  tests <- 
    zipWithM assertFloatEq
    [aim1, aim2, aim3, aim4, aim5]
    [t1, t2, t3, t4, t5]
  
  return (Test.list tests)
  
t1 = 
  "fun (a:*) ->"
  ++ "fix (f: pi (list a) (list a) nat ->list a) (xs:list a) (y:list a) -> "
  ++ "match xs with"
  ++ "| Nil -> fun (z:nat) -> y"
  ++ "| Cons x xs' -> fun (z:nat) -> Cons a x (f xs' y"
  ++ "(match z with | 0 -> 0 | Suc z' -> z' end)) end"
aim1 = 
  "fun (a:*) (xs:list a) (y:list a) -> "
  ++ "(fix (f: pi (list a) nat -> list a) (xs:list a) (z:nat) ->"
  ++ "match xs with"
  ++ "| Nil -> y"
  ++ "| Cons x xs' -> match z with"
    ++ "| 0 -> Cons a x (f xs' 0)"
    ++ "| Suc z' -> Cons a x (f xs' z') end end) xs"
    
t2 = "fix (f: pi nat nat ->nat) (x:nat) (y:nat) ->"
  ++ "match x with"
  ++ "| 0 -> y"
  ++ "| Suc x' -> Suc (f x' y) end"    
aim2 = "fun (x:nat) (y:nat) ->"
  ++ "(fix (f:pi nat->nat) (x:nat) ->"
  ++ "match x with"
  ++ "| 0 -> y"
  ++ "| Suc x' -> Suc (f x') end) x"

t3 = 
  "fun (a:*) (y:a) (xs:list a) ->"
  ++ "app a xs (Cons a y (Nil a))"
aim3 = 
  "fun (a:*) (y:a) ->"
  ++ "fix (app: pi (list a)->list a) (xs:list a) ->"
  ++ "match xs with"
  ++ "| Nil -> Cons a y (Nil a)"
  ++ "| Cons x xs' -> Cons a x (app xs') end"
  
t4 = 
  "fix (rev: pi (a:*) (list a) -> list a) (a:*) (xs:list a)->"
  ++ "match xs with"
  ++ "| Nil -> Nil a"
  ++ "| Cons x ys -> (app a) (rev a ys) (Cons a x (Nil a))"
  ++ "end"
aim4 = 
  "fun (a:*) -> "
  ++ "fix (f: pi (list a) -> list a) (xs: list a) ->"
  ++ "match xs with"
  ++ "| Nil -> Nil a"
  ++ "| Cons x ys -> app a (f ys) (Cons a x (Nil a))"
  ++ "end"
    
t5 = 
  "fix (rap: pi (a:*) a (list a) -> list a) (a:*) (y:a) (xs:list a)->"
  ++ "match xs with"
  ++ "| Nil -> Cons a y (Nil a)"
  ++ "| Cons x ys -> (app a) (rap a y ys) (Cons a x (Nil a))"
  ++ "end"
aim5 = 
  "fun (a:*) (y:a) -> "
  ++ "fix (f: pi (list a) -> list a) (xs: list a) ->"
  ++ "match xs with"
  ++ "| Nil -> Cons a y (Nil a)"
  ++ "| Cons x ys -> app a (f ys) (Cons a x (Nil a))"
  ++ "end"
  
