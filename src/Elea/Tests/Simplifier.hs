module Elea.Tests.Simplifier
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term, Note )

import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simplifier

assertSimplifyEq :: Note a => Term a -> Term a -> Test.Test
assertSimplifyEq x y =
  Test.label (show x' ++ " =?= " ++ show y')
  $ Test.assert (x' == y')
  where
  x' = Simplifier.run x
  y' = Simplifier.run y
  
tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  one_plus_one <- Test.term "+ 1 1"
  two <- Test.term "2"
  return (assertSimplifyEq one_plus_one two)
