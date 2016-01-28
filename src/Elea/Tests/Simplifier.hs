module Elea.Tests.Simplifier
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Testing ( Test )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Testing as Test
import qualified Elea.Transform.Prover as Prover

testProp :: String -> Test 
testProp prop_name = id
  . Test.testWithPrelude prop_name $ do
    all_props <- Test.loadFile properties_file
    let Just (Prop _ prop_t) = find ((== prop_name) . get propName) all_props
    prop_t' <-Test.printTimeTaken prop_name (Prover.run prop_t)
    Test.assertTermEq "" truth prop_t'
  
tests :: IO Test
tests = do
  prop_names <- Test.loadPropertyNamesFromFile properties_file
  return 
    . Test.label "Simplifier"
    . Test.list 
    $ map testProp prop_names

properties_file :: String
properties_file = "src/Elea/Tests/simplifier.elea"
