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

assertFloatEq :: Term -> Term -> Test.Test
assertFloatEq = Test.assertEq `on` Float.run

tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  
  t1 <- Test.term t1_str
  aim1 <- Test.term aim1_str
  let test1 = assertFloatEq aim1 t1
  
  t2 <- Test.term t2_str
  aim2 <- Test.term aim2_str
  let test2 = assertFloatEq aim2 t2
  
  return 
    $ Test.list
    [ test1, test2 ]
  where
  t1_str = 
    "fix (f:nat->nat->nat->nat) (x:nat) (y:nat) -> "
    ++ "match x with"
    ++ "| 0 -> fun (z:nat) -> y"
    ++ "| Suc x -> fun (z:nat) -> f x y"
    ++ "(match z with | 0 -> 0 | Suc z -> z end) end"
  aim1_str = 
    "fun (x:nat) (y:nat) -> "
    ++ "(fix (f:nat->nat->nat) (x:nat) (z:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x -> match z with"
      ++ "| 0 -> f x 0"
      ++ "| Suc z -> f x z end end) x"
      
  t2_str = "fix (f:nat->nat->nat) (x:nat) (y:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x' -> Suc (f x' y) end"
    
  aim2_str = "fun (x:nat) (y:nat) ->"
    ++ "(fix (f:nat->nat) (x:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x' -> Suc (f x') end) x"
      
