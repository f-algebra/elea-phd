module Elea.Tests.Simplifier
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term, Notes )

import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simplifier
import qualified Elea.Notes.Show as Show

assertSimplifyEq :: Show.HasNote a => Term a -> Term a -> Test.Test
assertSimplifyEq = Test.assertEq `on` Simplifier.run
  
tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  
  simple_t <- Test.term "(lam x -> (lam y z -> y) x)"
  simple_aim <- Test.term "(lam x y -> x)"
  let test1 = assertSimplifyEq simple_t simple_aim
  
  one_plus_one <- Test.term "+ 1 1"
  two <- Test.term "2"
  let test2 = assertSimplifyEq one_plus_one two
  
  two_times_two <- Test.term "* 2 2"
  two_plus_two <- Test.term "+ 2 2"
  let test3 = assertSimplifyEq two_times_two two_plus_two
  
  return
    $ Test.list [ test1, test2, test3 ]

