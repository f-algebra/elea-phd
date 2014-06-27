module Elea.Tests.Type
(
  tests
)
where

import Elea.Prelude
import Elea.Type hiding ( get )
import qualified Elea.Testing as Test

tests = Test.label "Types"
    $ Test.run $ do
  Test.loadPrelude
  
  Base nat <- Test._type "nat"
  Base bool <- Test._type "bool"
  
  let true = Constructor bool 0
      zero = Constructor nat 0
      succ = Constructor nat 1
  
  let t1 = Test.assert (isBaseCase true)
      t2 = Test.assert (isBaseCase zero)
      t3 = Test.assertEq [0] (recursiveArgs succ)
      t4 = Test.assert (isRecursive nat)
  
  return $ Test.list
    $ [ t1, t2, t3, t4 ]

