module Elea.Tests.All 
(
  runTests
)
where

import qualified Elea.Testing as Test
import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.Parser as Parser
import qualified Elea.Tests.Type as Type
import qualified Elea.Tests.Term as Term
import qualified Elea.Tests.Simplifier as Simplifier 
import qualified Elea.Tests.Context as Context
{-
import qualified Elea.Tests.Fusion as Fusion
-}

tests = Test.list
  [ Prelude.tests
  , Parser.tests
  , Type.tests
  , Term.tests
  , Simplifier.tests
  , Context.tests
  -- , Fusion.tests 
  ]
  
runTests :: IO ()
runTests = 
  Test.execute tests

