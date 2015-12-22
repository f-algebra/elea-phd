module Elea.Tests.Constraints
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Terms
import Elea.Type
import Elea.Show
import qualified Elea.Constraint as Constraint
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Evaluation as Eval
import qualified Elea.Simplifier as Simp
import qualified Elea.Fusion as Fusion
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

tests = Test.label "Constraints"
    $ Test.run $ do
  Test.loadPrelude
  Test.localVars "(x y: nat) (ys: list<nat>)" $ do
    sorted_xyys <- Test.term "is_sorted (Cons<nat> x (Cons<nat> y ys))"
    sorted_yys <- Test.term "is_sorted (Cons<nat> y ys)"
    x_leq_y <- Test.term "leq_nat x y"
    
    let sorted_xyys_c = Constraint.make Type.true sorted_xyys
        sorted_yys_c = Constraint.make Type.true sorted_yys
        x_leq_y_c = Constraint.make Type.true x_leq_y
         
        unf1 = Constraint.canUnfold sorted_xyys_c
        unf2 = Constraint.canUnfold sorted_yys_c
        test1 = Test.assertEq True unf1
        test2 = Test.assertEq False unf2
        
        sorted_xyys_cs = Constraint.unfold sorted_xyys_c
        expected3 = Just (Just [x_leq_y_c, sorted_yys_c])
        test3 = Test.assertEq expected3 sorted_xyys_cs
        
        sorted_yys_cs = Constraint.unfold sorted_yys_c
        test4 = Test.assertEq (Just (Just [sorted_yys_c])) sorted_yys_cs
        
    zero_leq_x <- Test.term "leq_nat 0 x"
    let zero_leq_c1 = Constraint.make Type.true zero_leq_x
        zero_leq_c2 = Constraint.make Type.false zero_leq_x
        test5 = Test.assertEq (Just Nothing) (Constraint.unfold zero_leq_c2)
        test6 = Test.assertEq (Just (Just [])) (Constraint.unfold zero_leq_c1)
        
    return (Test.list [test1, test2, test3, test4, test5, test6])
  
