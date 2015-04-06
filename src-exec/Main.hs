module Main
(
  module Elea.Prelude,
  main,
  test,
)
where

import Elea.Prelude
import Elea.Tests.All ( runTests )
import Text.Printf
import System.CPUTime
import qualified Elea.Testing as Test
--import qualified Elea.Tests.Fusion as Test
import qualified Elea.Terms as Term

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
    
test :: IO ()
test = time runTests

main = test
{-
run :: String -> IO ()
run prop_name = id 
    . Test.execute
    . Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src-lib/Elea/Tests/fusion.elea"
  mapM Test.checkEquation
    . filter ((== prop_name) . get Term.equationName)
    $ eqs
    -}
