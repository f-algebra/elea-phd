module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import Elea.Show ( showM )
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Testing as Test
import qualified Elea.Context as Context
import qualified Elea.Fusion as Fusion

assertFusionEq :: Int -> Test.M Test.Test
assertFusionEq n = do
  Just t <- Defs.lookup ("t" ++ show n)
  Just t' <- Defs.lookup ("t" ++ show n ++ "'")
  let ft  = t  |> Fusion.run |> Elea.run
  let ft' = t' |> Fusion.run |> Elea.run
  return (Test.assertEq ft' ft)

tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  Test.loadFile "fusion_tests.elea"
  liftM Test.list
    $ mapM assertFusionEq $
    [ 
    1..19
    ] ++ [110]

