module Elea.Tests.All 
(
  runTests
)
where

import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.ELisp as ELisp
import qualified Elea.Testing as Test

tests = Test.list
  [ Prelude.tests
  , ELisp.tests ]
  
runTests :: IO ()
runTests = 
  Test.execute tests
