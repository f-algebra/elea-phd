module Elea.Tests.Fusion
(
  tests
)
where

import Prelude()
import Elea.Prelude
import Elea.Term
import qualified Elea.Env as Env
import qualified Elea.Simplifier as Simp
import qualified Elea.Testing as Test
import qualified Elea.Fusion as Fusion
import qualified Elea.Equality as Equality
import qualified Elea.Definitions as Defs

checkEquation :: Equation -> Test.M Test.Test
checkEquation (Equals name bs t1 t2) = id
  . liftM (Test.label name)
  . Env.emptyT
  . Env.bindMany bs $ do
    t1' <- Fusion.run t1
    t2' <- Fusion.run t2
    Test.assertProvablyEq t1' t2'

  
tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/fusion.elea"
  mapM checkEquation
    . filter ((/= "test add") . get equationName)
    $ eqs


