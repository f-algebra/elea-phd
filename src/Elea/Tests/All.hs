module Elea.Tests.All 
(
  runTests
)
where

import qualified Elea.Testing as Test
import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.Parser as Parser
-- import qualified Elea.Tests.Simplifier as Simplifier
-- import qualified Elea.Tests.Context as Context
-- import qualified Elea.Tests.Floating as Float

tests = Test.list
  [ Prelude.tests
  , Parser.tests 
--  , Simplifier.tests
--  , Context.tests
 -- , Float.tests
  ]
  
runTests :: IO ()
runTests = 
  Test.execute tests

