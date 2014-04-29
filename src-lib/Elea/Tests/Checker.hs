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
  
  Test.localVars "(x y z: nat) (xs: list<nat>) (t: tree<nat>)" $ do
    x_leq_y <- Test.simplifiedTerm "leq_nat x y"
    y_leq_z <- Test.simplifiedTerm "leq_nat y z"
    x_leq_z <- Test.simplifiedTerm "leq_nat x z"
    true <- Test.term "True"
    
    Base bool <- Test._type "bool"
    let xleqy_c = Constraint.fromMatch (x_leq_y, true)
        yleqz_c = Constraint.fromMatch (y_leq_z, true)
        leq_constrs = Set.fromList [xleqy_c, yleqz_c]
        Just true' = Checker.constrainedToConstant leq_constrs x_leq_z
        test_leq = Test.assertEq true true'
        
    not_elem_x_xs <- Test.simplifiedTerm "not (elem_nat x xs)"
    elem_xs_cons <- 
      Test.fusedTerm "elem_nat x (append<nat> xs (Cons<nat> y Nil<nat>))"
    x_eq_y <- Test.simplifiedTerm "eq_nat x y"
    let not_elem_c = Constraint.fromMatch (not_elem_x_xs, true)
        elem_constrs = Set.fromList [not_elem_c]
        Just x_eq_y' = Checker.constrainedToConstant elem_constrs elem_xs_cons
        test_elem = Test.assertEq x_eq_y x_eq_y'
    
    rightleq_x <- Test.simplifiedTerm "rightmost_leq x t"
    sorted_t <- Test.simplifiedTerm "sorted_tree t"
    let srtd_con = Set.fromList [Constraint.fromMatch (sorted_t, true)]
        mby_rightleq_const = Checker.constrainedToConstant srtd_con rightleq_x
        test_tree = Test.assertEq Nothing mby_rightleq_const
      
    return (Test.list [ test_leq, test_elem, test_tree ])
