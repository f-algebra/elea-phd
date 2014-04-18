module Elea.Tests.Type
(
  tests
)
where

import Elea.Prelude
import Elea.Type
import qualified Elea.Testing as Test

tests = Test.label "Types"
    $ Test.run $ do
  Test.loadPrelude
  
  Base nat <- Test._type "nat"
  Base bool <- Test._type "bool"
  
  let t1 = Test.assert (isBaseCase bool 0)
      t2 = Test.assert (isBaseCase nat 0)
      t3 = Test.assertEq [0] (recursiveArgs nat 1)
      t4 = Test.assert (isRecursive nat)
  
  return $ Test.list
    $ [ t1, t2, t3, t4 ]

