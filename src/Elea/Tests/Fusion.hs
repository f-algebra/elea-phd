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
import qualified Elea.Terms as Term
import qualified Elea.Context as Context
import qualified Elea.Fusion as Fusion
import qualified Elea.Floating as Float

assertFusionEq :: Int -> Test.M Test.Test
assertFusionEq n = do
  Just t <- Defs.lookup ("t" ++ show n)
  Just t' <- Defs.lookup ("t" ++ show n ++ "'")
  let ft  = fuse t
  let ft' = fuse t'
  return (Test.assertEq ft' ft)
  where
  fuse = Elea.run . Fusion.run

tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  Test.loadFile "fusion_tests.elea"
  liftM Test.list
    $ mapM assertFusionEq $
    [
     5421
    ]
   -- ++ [1..5] ++ [9..18]  ++ [110] 

