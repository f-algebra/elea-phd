module Elea.Tests.All 
(
  runTests
)
where

import qualified Elea.Testing as Test
import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.Lisp as Lisp
import qualified Elea.Tests.Parser as Parser
import qualified Elea.Tests.Simplifier as Simplifier

tests = Test.list
  [ Prelude.tests
  , Lisp.tests
  , Parser.tests 
  , Simplifier.tests
  ]
  
runTests :: IO ()
runTests = 
  Test.execute tests

