module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )
import Elea.Show ( showM )
import qualified Elea.Testing as Test
import qualified Elea.Context as Context
import qualified Elea.Fusion as Fusion

assertFusionEq :: String -> String -> Elea Test.Test
assertFusionEq aim_s t_s = do
  t <- Test.term t_s
  aim <- Test.term aim_s
  t_s <- showM t
  t' <- Fusion.run t
  aim' <- Fusion.run aim
  return (Test.assertEq aim' t')

tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude

  tests <- id
    . mapM (uncurry assertFusionEq)
   -- . take 1
    $ zip
    [ aim1, aim2, aim3, aim4, aim5, aim6, aim7 ]
    [ t1, t2, t3, t4, t5, t6, t7 ]
    
  return (Test.list tests)

t1 =  
  "fun (a:*) (y:a) (zs:list a) -> "
  ++ "rev a (app a zs (Cons a y (Nil a)))" 
aim1 = 
  "fun (a:*) (y:a) (xs:list a) -> "
  ++ "Cons a y ((fix (f: pi (list a)->list a) (xs:list a) ->"
  ++ "match xs with"
  ++ "| Nil -> Nil a"
  ++ "| Cons x xs -> app a (f xs) (Cons a x (Nil a)) end) xs)"
  
t2 = "fun (x:nat) (y:nat) -> add x (Suc y)"
aim2 = "fun (x:nat) (y:nat) -> Suc (add x y)"

t3 = "fun (a:*) (xs:list a) (ys:list a) (zs:list a) ->"
  ++ "((app a) (app a xs ys)) zs"
aim3 = "fun (a:*) (xs:list a) (ys:list a) (zs:list a) ->"
  ++ "app a xs (app a ys zs)"
  
t4 = "fun (x:nat)(y:nat)(z:nat) -> add (add x y) z"
aim4 = "fun (x:nat) (y:nat) (z:nat) -> add x (add y z)"

t5 = "fix (f:pi nat -> bool) (x:nat) ->"
  ++ "match x with | 0 -> True | Suc x' -> f x' end"
aim5 = "fun (x:nat) -> True"

t6 = "fun (n:nat) (xs:list nat) -> "
  ++ "count n (app nat xs (Cons nat n (Nil nat)))"
aim6 = "fun (n:nat) (xs:list nat) -> Suc (count n xs)"

t7 = "fun (n:nat) (xs:list nat) -> count n (rev nat xs)"
aim7 = "fun (n:nat) (xs:list nat) -> count n xs"

