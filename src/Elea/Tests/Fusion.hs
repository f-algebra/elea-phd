module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Env as Env
import qualified Elea.Simplifier as Simp
import qualified Elea.Testing as Test
import qualified Elea.Fusion as Fusion

checkEquation :: Equation -> Test.Test
checkEquation (Equals name bs t1 t2) = id
  . Test.label name
  . Env.empty
  . Env.bindMany bs $ do
    t1' <- Fusion.run t1
    t2' <- Fusion.run t2
    return (Test.assertEq t2' t1')
  
tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src/Elea/Tests/fusion.elea"
  return
    . map checkEquation
 --   . filter ((== "count reverse") . get equationName)
    $ eqs

