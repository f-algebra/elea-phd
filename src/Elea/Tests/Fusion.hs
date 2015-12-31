module Elea.Tests.Fusion
(
  tests  
)
where

import Elea.Prelude
import Elea.Term
import Elea.Testing ( Test )
import qualified Elea.Monad.Env as Env
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Testing as Test
import qualified Elea.Type.Ext as Type
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs

testProp :: Prop -> Test.M ()
testProp (Prop name t) = do
  t' <- Fusion.run t
  Test.assertTermEq (printf "prop %s" name) truth t'
  
tests :: Test
tests = id
  . Test.testWithPrelude "Simplifier" $ do
      props <- Test.loadFile "src/Elea/Tests/fusion.elea"
      mapM_ testProp 
       -- . filter ((== "beta2") . get equationName)
        $ props

