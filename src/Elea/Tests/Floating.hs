module Elea.Tests.Floating
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp

assertFloatEq :: Int -> Elea Test.Test
assertFloatEq n = do
  Just t <- Defs.lookup ("t" ++ show n)
  Just t' <- Defs.lookup ("t" ++ show n ++ "'")
  return (Test.assertEq (Float.run t') (Float.run t))

tests = Test.label "Floating"
    $ Test.run $ do
  Test.loadPrelude
  Test.loadFile "floating_tests.elea"

  liftM Test.list
    $ mapM assertFloatEq [1..8]

