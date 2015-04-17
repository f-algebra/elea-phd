module Elea.Tests.Fusion
(
  tests, checkEquation
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Testing as Test
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Equality as Equality
import qualified Elea.Monad.Definitions as Defs

checkEquation :: Equation -> Test.M Test.Test
checkEquation (Equals name bs t) = id
  . liftM (Test.label name)
  . Env.bindMany bs $ do
    t' <- Fusion.run t
    let (_, t_body) = flattenLam t'
   -- Test.assertProvablyEq t1' t2'
    return (Test.assertTermEq true t_body)
    
tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/fusion.elea"
  mapM checkEquation
    . filter ((== "revapp cons") . get equationName)
    $ eqs

