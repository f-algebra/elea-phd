module Elea.Tests.Floating
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp

assertFloatEq :: Term -> Term -> Test.Test
assertFloatEq = Test.assertEq `on` Float.run

tests = Test.label "Floating"
    $ Test.run $ do
  Test.loadPrelude
  
  t1 <- Test.term t1_str
  aim1 <- Test.term aim1_str
  let test1 = assertFloatEq aim1 t1
  
  t2 <- Test.term t2_str
  aim2 <- Test.term aim2_str
  let test2 = assertFloatEq aim2 t2
  
  t3 <- Test.term t3_str
  aim3 <- Test.term aim3_str
  let test3 = assertFloatEq aim3 t3
  
  return 
    $ Test.list
    [ test1, test2, test3 ]
  where
  t1_str = 
    "fun (a:*) ->"
    ++ "fix (f: pi (list a) (list a) nat ->list a) (xs:list a) (y:list a) -> "
    ++ "match xs with"
    ++ "| Nil -> fun (z:nat) -> y"
    ++ "| Cons x xs' -> fun (z:nat) -> Cons a x (f xs' y"
    ++ "(match z with | 0 -> 0 | Suc z' -> z' end)) end"
  aim1_str = 
    "fun (a:*) (xs:list a) (y:list a) -> "
    ++ "(fix (f: pi (list a) nat -> list a) (xs:list a) (z:nat) ->"
    ++ "match xs with"
    ++ "| Nil -> y"
    ++ "| Cons x xs' -> match z with"
      ++ "| 0 -> Cons a x (f xs' 0)"
      ++ "| Suc z' -> Cons a x (f xs' z') end end) xs"
      
  t2_str = "fix (f: pi nat nat ->nat) (x:nat) (y:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x' -> Suc (f x' y) end"    
  aim2_str = "fun (x:nat) (y:nat) ->"
    ++ "(fix (f:pi nat->nat) (x:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x' -> Suc (f x') end) x"
 
  t3_str = 
    "fun (a:*) (y:a) (xs:list a) ->"
    ++ "app a xs (Cons a y (Nil a))"
  aim3_str = 
    "fun (a:*) (y:a) ->"
    ++ "fix (app: pi (list a)->list a) (xs:list a) ->"
    ++ "match xs with"
    ++ "| Nil -> Cons a y (Nil a)"
    ++ "| Cons x xs' -> Cons a x (app xs') end"
      
