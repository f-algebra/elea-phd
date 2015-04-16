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
  liftM Test.list (mapM doTest hs)

  where
  hs = 
    [ ( 2, "fun (x: nat) -> x" )
      
    , ( 3,  "fun (f: nat -> nat) (x: nat) -> x" )
      
    , ( 7
      , "fun (x: bool) -> if (if x then True else False) then True else False" )
    ]
    
  doTest (h, s) = do
    t <- Test.term s
    return (Test.assertEq h (Height.get t))
