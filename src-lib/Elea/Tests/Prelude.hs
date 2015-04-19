module Elea.Tests.Prelude 
(
  tests
) 
where

import Elea.Prelude hiding ( assert )
import Test.HUnit ( Test (..), assert )
import qualified Data.Set as Set

tests = TestLabel "Prelude"
  $ TestList 
  [ test_deleteIndices
  , test_takeIndices
  , test_isNub
  , test_nubOrd
  , test_removeAll ]
  
testCase = TestCase . assert
testList = TestList . map testCase

test_deleteIndices = testCase
  $ deleteIndices [1, 3, 6] [0..7] == [0, 2, 4, 5, 7]
  
test_takeIndices = testCase
  $ takeIndices [1, 3, 6] [0..7] == [1, 3, 6]
  
test_isNub = testList $ 
  [ isNub [0, 2, 1]
  , not $ isNub [3, 1, 2, 1, 5] 
  , isNub ([] :: [Int]) ]
  
test_nubOrd = testList $
  [ nubOrd ([] :: [Int]) == []
  , nubOrd [4, 7, 2, 4, 5, 7, 1] == [4, 7, 2, 5, 1] ]
  
test_removeAll = testList $
  [ removeAll (Set.fromList [1, 3]) [0..5] == [0, 2, 4, 5]
  , removeAll (Set.fromList [0..9]) [1..10] == [] ]

