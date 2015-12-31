module Elea.Tests.All 
(
  all
)
where

import qualified Test.Framework.Providers.HUnit as HUnit
import qualified Test.Framework

import Elea.Prelude hiding ( all )
import qualified Elea.Testing as Test
import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.Type as Type
import qualified Elea.Tests.Term as Term
import qualified Elea.Tests.Simplifier as Simplifier 
--import qualified Elea.Tests.Inventor as Inventor
-- import qualified Elea.Tests.Constraints as Constraints
import qualified Elea.Tests.Fusion as Fusion
--import qualified Elea.Tests.UMap as UMap
--import qualified Elea.Tests.Checker as Checker

all :: [Test.Framework.Test]
all = id
  . HUnit.hUnitTestToTests
  . Test.list
  $ [ Prelude.tests
    , Type.tests
    , Term.tests
  --  , UMap.tests
  --  , Checker.tests 
    , Simplifier.tests
   -- , Constraints.tests
   -- , Context.tests
   -- , Inventor.tests

   -- Can't run these until I improve the memory usage
   -- , Fusion.tests  
    ]
