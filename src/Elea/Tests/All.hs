module Elea.Tests.All 
(
  all
)
where

import Elea.Prelude hiding ( all )
import Elea.Testing ( Test )
import qualified Elea.Testing as Test
import qualified Elea.Tests.Prelude as Prelude
import qualified Elea.Tests.Type as Type
import qualified Elea.Tests.Term as Term
import qualified Elea.Tests.Simplifier as Simplifier 
import qualified Elea.Tests.Rewrite.Drive as Drive
-- import qualified Elea.Tests.Rewrite.Supercompile as Supercompile
--import qualified Elea.Tests.Inventor as Inventor
-- import qualified Elea.Tests.Constraints as Constraints
import qualified Elea.Tests.Fusion as Fusion
--import qualified Elea.Tests.UMap as UMap
--import qualified Elea.Tests.Checker as Checker

all :: IO Test
all = id
  . liftM Test.list
  $ sequence 
  [ return Prelude.tests
--  , return Type.tests
  , Term.tests
--  , UMap.tests
--  , Checker.tests 
 -- , Constraints.tests
 -- , Context.tests
 -- , Inventor.tests
 -- , Simplifier.tests
 -- , Fusion.tests  
  , Drive.tests
--  , Supercompile.tests
  ]
