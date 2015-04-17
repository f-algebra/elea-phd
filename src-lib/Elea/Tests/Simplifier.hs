module Elea.Tests.Simplifier
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Testing as Test
import qualified Elea.Transform.Simplify as Simp

checkEquation :: Equation -> Test.Test
checkEquation (Equals name bs t) = id
  . Test.label name
  $ Test.assertTermEq true t_body
  where
  (_, t_body) = flattenLam (Simp.run t)
  
tests = id
    . Test.label "Simplifier"
    . Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/simplifier.elea"
  return 
    . map checkEquation 
   -- . filter ((== "beta2") . get equationName)
    $ eqs

