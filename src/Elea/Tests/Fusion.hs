module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Context as Context
import qualified Elea.Fusion as Fusion

tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude

  revapp <- Test.term revapp_s
  revapp_aim <- liftM Float.run (Test.term revapp_aim_s)
  revapp_fused <- Fusion.run revapp
  let test1 = Test.assertEq revapp_aim revapp_fused
  
  return
    $ Test.list [ test1 ]
  where
  revapp_s =  
    "fun (a:*) (y:a) (zs:list a) -> "
    ++ "rev a (app a zs (Cons a y (Nil a)))" 

  revapp_aim_s = 
    "fun (a:*) (y:a) (xs:list a) -> "
    ++ "Cons a y ((fix (f: pi (list a)->list a) (xs:list a) ->"
    ++ "match xs with"
    ++ "| Nil -> Nil a"
    ++ "| Cons x xs -> app a (f xs) (Cons a x (Nil a)) end) xs)"

