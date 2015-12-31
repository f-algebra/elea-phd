module Elea.Tests.Prelude 
(
  tests
) 
where

import Elea.Prelude hiding ( assert )
import Test.HUnit ( Test (..) )
import qualified Test.HUnit as HUnit
import qualified Data.Set as Set

tests :: Test
tests = TestLabel "Prelude" testPrelude

testPrelude :: Test
testPrelude = TestCase $ do
  HUnit.assertEqual "delete indices" [0, 2, 4, 5, 7] (deleteIndices [1, 3, 6] [0..7])
  HUnit.assertEqual "take indices" [1, 3, 6] (takeIndices [1, 3, 6] [0..7])
  HUnit.assertEqual "removeAll 1" [0, 2, 4, 5] (removeAll (Set.fromList [1, 3]) [0..5])
  HUnit.assertEqual "removeAll 2" [] (removeAll (Set.fromList [0..9]) [1..10])
