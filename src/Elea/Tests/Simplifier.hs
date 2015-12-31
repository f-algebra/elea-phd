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

testProp :: Prop -> Test.M ()
testProp (Prop name t) = do
  t' <- Prover.run t
  Test.assertTermEq (printf "prop %s" name) truth t'
  
tests :: Test
tests = id
  . Test.testWithPrelude "Simplifier" $ do
      props <- Test.loadFile "src/Elea/Tests/simplifier.elea"
      mapM_ testProp 
       -- . filter ((== "beta2") . get equationName)
        $ props

