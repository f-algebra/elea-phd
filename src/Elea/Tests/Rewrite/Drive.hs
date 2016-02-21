module Elea.Tests.Rewrite.Drive (
  tests
) where

import Elea.Prelude
import Elea.Term
import Elea.Testing ( Test )
import qualified Elea.Testing as Test
import qualified Elea.Rewrite.Drive as Drive

testProp :: String -> Test 
testProp prop_name = id
  . Test.testWithPrelude prop_name $ do
    all_props <- Test.loadFile properties_file
    let Just (Prop _ prop_t) = find ((== prop_name) . get propName) all_props
    prop_t' <- Test.recordTimeTaken prop_name (Drive.rewrite prop_t)
    Test.assertTermEq "" truth prop_t'
  
tests :: IO Test
tests = do
  prop_names <- Test.loadPropertyNamesFromFile properties_file
  return 
    . Test.label "Driver"
    . Test.list 
    $ map testProp prop_names

properties_file :: String
properties_file = "src/Elea/Tests/Rewrite/drive.elea"
