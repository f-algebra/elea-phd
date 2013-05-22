module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )
import Elea.Show ( showM )
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Testing as Test
import qualified Elea.Context as Context
import qualified Elea.Fusion.Simplifier as Fusion

assertFusionEq :: Int -> Elea Test.Test
assertFusionEq n = do
  Just t <- Defs.lookup ("t" ++ show n)
  Just t' <- Defs.lookup ("t" ++ show n ++ "'")
  ft <- Fusion.run t
  ft' <- Fusion.run t'
  return (Test.assertEq ft' ft)

tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  Test.loadFile "fusion_tests.elea"
  liftM Test.list
    $ mapM assertFusionEq [1..10]

