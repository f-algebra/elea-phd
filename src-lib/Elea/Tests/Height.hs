module Elea.Tests.Height
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Term.Height as Height
import qualified Elea.Testing as Test
import qualified Data.Poset as Partial

tests = Test.label "Height"
    $ Test.run $ do
  Test.loadPrelude
  liftM Test.list (mapM doTest gt)

  where
  gt = 
    [ ( (2, 1), "fun (x: nat) -> x" )
      
    , ( (3, 1),  "fun (f: nat -> nat) (x: nat) -> x" )
      
    , ( (7, 4)
      , "fun (x: bool) -> if (if x then True else False) then True else False" )
    ]
    
  doTest ((h, n), s) = do
    t <- Test.term s
    return (Test.assertEq (Height.Height h n) (Height.get t))
