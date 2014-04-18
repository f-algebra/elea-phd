module Main where

import Elea.Prelude
import Elea.Tests.All ( runTests )
import Text.Printf
import System.CPUTime

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
