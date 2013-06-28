module Elea.Tests.Term
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp
import qualified Data.Set as Set

tests = Test.label "Terms"
    $ Test.run $ do
  Test.loadPrelude
  
  nat_ty <- Test.term "nat"
  bool_ty <- Test.term "bool"
  list_ty <- liftM Simp.run (Test.term "list nat")
  tree_ty <- Test.term
    "ind t:* with | Leaf: t | Node: pi t bool t -> t end"
  
  let node = unflattenApp [Inj 1 tree_ty, Var 2, Var 1, Var 0]
      test0 = altPattern tree_ty 1 `Test.assertEq` node
  
      test1 = Test.assert (isRecursiveInd tree_ty)
      test2 = Test.assert (not (isRecursiveInd bool_ty))
      test3 = Test.assert (isBaseCase list_ty 0)
      test4 = Test.assert (not (isBaseCase tree_ty 1))
      test5 = Set.fromList [0, 2] `Test.assertEq` recursiveInjArgs tree_ty 1
      test6 = Set.empty `Test.assertEq` recursiveInjArgs list_ty 0
      test7 = Set.singleton 1 `Test.assertEq` recursiveInjArgs list_ty 1
      
  Lam _ list1 <- id
    . liftM Simp.run
    $ Test.term "fun (x:nat) -> Cons nat x (Nil nat)"
  list2 <- id
    . liftM Simp.run
    $ Test.term "fun (xs:list nat) -> Cons nat 2 (Cons nat 1 xs)"
  let test8 = Test.assert (isFinite list1) 
      test9 = Test.assert (not (isFinite list2))
      
  return 
    $ Test.list
    [ test0, test1, test2, test3, test4
    , test5, test6, test7, test8, test9 ]
  
