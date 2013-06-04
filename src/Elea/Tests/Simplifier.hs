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
  return $ assertSimplifyEq simple_t simple_aim


