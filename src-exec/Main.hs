module Main
(
  module Elea.Prelude,
  main,
  test,
  run
)
where

import Elea.Prelude
import Elea.Tests.All ( runTests )
import Elea.Show ( showM )
import Text.Printf
import System.CPUTime
import qualified Elea.Testing as Test
--import qualified Elea.Tests.Fusion as Test
import qualified Elea.Term.Ext as Term
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Fedd as Fedd

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10**12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
    
test :: IO ()
test = time runTests

main = test

run :: String -> IO ()
run term_def = putStrLn output
  where
  output = Fedd.eval $ do
    Test.loadPrelude
    term <- Test.term term_def
    term' <- Fusion.run term
    term_s' <- showM term'
    term_s <- showM term
    return ("\n\n[hihi]" ++ term_s ++ "\n\n==>\n\n" ++ term_s')
  
    
