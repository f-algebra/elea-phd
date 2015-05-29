module Elea.Tests.Simplifier
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Testing as Test
import qualified Elea.Transform.Prover as Prover

checkProp :: (Env.Read m, Defs.Has m) => Prop -> m Test.Test
checkProp (Prop name t) = id
  . liftM (Test.label name)
  . Test.assertTruth
  $ Prover.run t
  
tests = id
    . Test.label "Simplifier"
    . Test.run $ do
  Test.loadPrelude  
  eqs <- Test.loadFile "src-lib/Elea/Tests/simplifier.elea"
  mapM checkProp 
   -- . filter ((== "beta2") . get equationName)
    $ eqs

