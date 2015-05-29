module Elea.Tests.Fusion
(
  tests, checkProp  
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Testing as Test
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs

checkProp :: Prop -> Test.M Test.Test
checkProp (Prop name t) = 
  liftM (Test.label name) $ do
    t' <- Fusion.run t
    Test.assertTruth t'
    
tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/fusion.elea"
  mapM checkProp
    . filter ((== "test") . get propName)
    $ eqs

