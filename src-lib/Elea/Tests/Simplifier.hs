module Elea.Tests.Simplifier
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp

checkEquation :: Equation -> Test.Test
checkEquation (Equals name bs t1 t2) = id
  . Test.label name
  $ Test.assertSimpEq t1 t2
  
tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/simplifier.elea"
  return (map checkEquation eqs)

