module Elea.Tests.Simplifier
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )

import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simplifier

assertSimplifyEq :: Term -> Term -> Test.Test
assertSimplifyEq = Test.assertEq `on` Simplifier.run
  
tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  
  simple_t <- Test.term "fun (a:*) (x:a) -> (fun (y:a) (z:a) -> y) x"
  simple_aim <- Test.term "fun (a:*) (x:a) (y:a) -> x"
  let test1 = assertSimplifyEq simple_t simple_aim
  
  one_plus_one <- Test.term "add 1 1"
  two <- Test.term "2"
  let test2 = assertSimplifyEq one_plus_one two
  
  two_times_two <- Test.term "mul 2 2"
  two_plus_two <- Test.term "add 2 2"
  let test3 = assertSimplifyEq two_times_two two_plus_two
  
  rev_simple <- Test.term
    "rev nat (Cons nat 1 (Cons nat 2 (Nil nat)))"
  rev_aim <- Test.term
    "Cons nat 2 (Cons nat 1 (Nil nat))"
  let test4 = assertSimplifyEq rev_aim rev_simple
  
  return
    $ Test.list [ test1, test2, test3, test4 ]

