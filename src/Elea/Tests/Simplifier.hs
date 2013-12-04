module Elea.Tests.Simplifier
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Env as Env
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp

checkEquation :: Equation -> Test.Test
checkEquation (Equals name bs t1 t2) = id
  . Test.label name
  . Env.empty
  . Env.bindMany bs $ do
    t1' <- Simp.run t1
    t2' <- Simp.run t2
    return (Test.assertEq t1' t2')
  
tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src/Elea/Tests/simplifier.elea"
  return (map checkEquation eqs)

