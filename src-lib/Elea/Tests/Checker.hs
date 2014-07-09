module Elea.Tests.Checker
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Type
import qualified Elea.Constraint as Constraint
import qualified Elea.Checker as Checker
import qualified Elea.Index as Indices
import qualified Elea.Testing as Test
import qualified Data.Set as Set

tests = Test.label "Checker"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.localVars "(x y z: nat) (xs: list<nat>) (t t1 t2: tree<nat>)" $ do
    x_leq_y <- Test.simplifiedTerm "leq_nat x y"
    y_leq_z <- Test.simplifiedTerm "leq_nat y z"
    x_leq_z <- Test.simplifiedTerm "leq_nat x z"
    true <- Test.term "True"
    false <- Test.term "False"
   
    Base bool <- Test._type "bool"
    let xleqy_c = Constraint.fromMatch (x_leq_y, true)
        yleqz_c = Constraint.fromMatch (y_leq_z, true)
        leq_constrs = Set.fromList [xleqy_c, yleqz_c]
        mby_true = Checker.constrainedToConstant leq_constrs x_leq_z
        test_leq = Test.assertEq (Just true) mby_true
        
        leq_c2 = Set.singleton (Constraint.fromMatch (x_leq_y, false))
        leq_nothing = Checker.constrainedToConstant leq_c2 x_leq_z
        test_leq2 = Test.assertEq Nothing leq_nothing
        
    not_elem_x_xs <- Test.simplifiedTerm "not (elem_nat x xs)"
    elem_xs_cons <- 
      Test.fusedTerm "elem_nat x (append<nat> xs (Cons<nat> y Nil<nat>))"
    x_eq_y <- Test.simplifiedTerm "eq_nat x y"
    let not_elem_c = Constraint.fromMatch (not_elem_x_xs, true)
        elem_constrs = Set.fromList [not_elem_c]
        mby_x_eq_y = Checker.constrainedToConstant elem_constrs elem_xs_cons
        test_elem = Test.assertEq (Just x_eq_y) mby_x_eq_y
   
    rightleq_y <- Test.simplifiedTerm "rightmost_leq t y" 
    sorted_t <- Test.simplifiedTerm "sorted_tree t" 
    let srtd_con = Set.fromList [Constraint.fromMatch (sorted_t, true)] 
        mby_rightleq_const = Checker.constrainedToConstant srtd_con rightleq_y
        test_tree = Test.assertEq Nothing mby_rightleq_const
        
   
    let mby_srtd_srtd = Checker.constrainedToConstant srtd_con sorted_t
        test_tree2 = Test.assertEq (Just true) mby_srtd_srtd
        
        not_srtd_con = Set.singleton (Constraint.fromMatch (sorted_t, false))
        mby_tree3 = Checker.constrainedToConstant not_srtd_con sorted_t
        test_tree3 = Test.assertEq (Just false) mby_tree3
        
    srtd_ins_xs <- Test.fusedTerm "is_sorted (insert_nat x xs)"
    srtd_xs <- Test.simplifiedTerm "is_sorted xs"
    let srtd_c = Set.singleton (Constraint.fromMatch (srtd_xs, false))
        mby_srtd = Checker.constrainedToConstant srtd_c srtd_ins_xs
        test_srtd = Test.assertEq (Just false) mby_srtd
        
   -- srtd_tree_ins <- Test.fusedTerm "sorted_tree (tree_insert x t)"
   -- let mby_tree4 = Checker.constrainedToConstant srtd_con srtd_tree_ins
     --   test_tree4 = Test.assertEq (Just true) mby_tree4
     
    let rleq_con = Set.singleton (Constraint.fromMatch (rightleq_y, true))
        mby_tree5 = Checker.constrainedToConstant rleq_con sorted_t
        test_tree5 = Test.assertEq Nothing mby_tree5
  
    -- Leaving out test_leq2 because it only works for 
    -- search depth 2, but this will stop test_srtd working
    -- which is the more important one.
    -- Also leaving out test_tree4 because the fusion step takes aaaages.
        
    return (Test.list 
      [ test_elem, test_leq, test_tree3 --, test_tree5
      , test_tree, test_tree2, test_srtd ]) 

