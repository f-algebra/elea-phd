module Elea.Tests.Term
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Terms
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp
import qualified Elea.Floating as Float
import qualified Elea.Monad.Elea as Elea
import qualified Data.Set as Set

tests = Test.label "Terms"
    $ Test.run $ do
  Test.loadPrelude
  
  nat_ty <- Test.term "nat"
  bool_ty <- Test.term "bool"
  list_ty <- Test.term "list nat"
  tree_ty <- Test.term "tree nat"
  
  let node = App (Inj 1 tree_ty) [Var 2, Var 1, Var 0]
      test0 = altPattern tree_ty 1 `Test.assertEq` node
  
      test1 = Test.assert (isRecursiveInd tree_ty)
      test2 = Test.assertNot (isRecursiveInd bool_ty)
      test3 = Test.assert (isBaseCase list_ty 0)
      test4 = Test.assertNot (isBaseCase tree_ty 1)
      test5 = Set.fromList [1, 2] `Test.assertEq` recursiveInjArgs tree_ty 1
      test6 = Set.empty `Test.assertEq` recursiveInjArgs list_ty 0
      test7 = Set.singleton 1 `Test.assertEq` recursiveInjArgs list_ty 1      
  
  Lam _ list1 <- Test.term "fun (x:nat) -> Cons nat x (Nil nat)"
  list2 <- Test.term "fun (xs:list nat) -> Cons nat 2 (Cons nat 1 xs)"
  let test8 = Test.assert (isFinite list1) 
      test9 = Test.assertNot (isFinite list2)
      
  take_fix@(Fix {}) <- Test.term "take"
  Lam _ height_fix@(Fix {}) <- Test.term "height"
  Lam _ flat_fix@(Fix {}) <- Test.term "flatten"
  Lam _ mirror_fix@(Fix {}) <- Test.term "mirror"
  ins_fix@(Fix {}) <- Test.term "insert"
  srtd_fix@(Fix {}) <- Test.term "sorted"
  eq_fix@(Fix {}) <- Test.term "eq_nat"
  
  let test10 = Test.assert (isProductive take_fix)
      test11 = Test.assert (isProductive height_fix)
      test12 = Test.assertNot (isProductive flat_fix)
      test13 = Test.assert (isProductive mirror_fix)
      test14 = Test.assert (isProductive ins_fix)
      test15 = Test.assertNot (isProductive srtd_fix)
      test16 = Test.assertNot (isProductive eq_fix)
    
  nil <- Test.term "Nil nat"
  one_nil <- Test.term "Cons nat 2 (Nil nat)"
  tree <- Test.term "Node nat 1 (Node nat 2 (Leaf nat) (Leaf nat)) (Leaf nat)"
  
  let test17 = Test.assertEq 0 (maximumInjDepth take_fix)
      test18 = Test.assertEq 0 (minimumInjDepth take_fix)
      test19 = Test.assertEq 0 (maximumInjDepth nil)
      test20 = Test.assertEq 1 (minimumInjDepth one_nil)
      test21 = Test.assertEq 1 (minimumInjDepth tree)
      test22 = Test.assertEq 2 (maximumInjDepth tree)
      
  let test23 = Test.assertEq 1 (recursionDepth mirror_fix)
      test24 = Test.assertEq 2 (recursionDepth srtd_fix)
      test25 = Test.assertEq 1 (recursionDepth ins_fix)
      
  return
    $ Test.list
    [ test0, test1, test2, test3, test4
    , test5, test6, test7, test8, test9 
    , test10, test11, test12, test13, test14, test15, test16
    , test17, test18, test19, test20, test21, test22
    , test23, test24, test25 ]
  
