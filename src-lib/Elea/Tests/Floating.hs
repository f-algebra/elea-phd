module Elea.Tests.Floating
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term )
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp

assertFloatEq :: Int -> Test.M Test.Test
assertFloatEq n = do
  Just t <- Defs.lookup ("t" ++ show n)
  Just t' <- Defs.lookup ("t" ++ show n ++ "'")
  let ft  = t  |> Float.run |> Elea.run
  let ft' = t' |> Float.run |> Elea.run
  return (Test.assertEq ft' ft)

tests = Test.label "Floating"
    $ Test.run $ do
  Test.loadPrelude
  Test.loadFile "floating_tests.elea"

  liftM Test.list
    $ mapM assertFloatEq 
    $ [1..8] ++ [10..17]

