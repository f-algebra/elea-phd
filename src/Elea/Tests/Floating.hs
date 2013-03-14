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
  
  return 
    $ Test.list
    [ test1 ]
  where
  t1_str = 
    "fix (f:nat->nat->nat) (x:nat) -> "
    ++ "match x with"
    ++ "| 0 -> fun (y:nat) -> y"
    ++ "| Suc x -> fun (y:nat) -> f x "
    ++ "(match y with | 0 -> 0 | Suc y -> y end) end"
  aim1_str = 
    "fix (f:nat->nat->nat) (x:nat) (y:nat) ->"
    ++ "match x with"
    ++ "| 0 -> y"
    ++ "| Suc x -> match y with"
      ++ "| 0 -> f x 0"
      ++ "| Suc y -> f x y end end"
    
