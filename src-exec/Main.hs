module Main
(
  module Elea.Prelude,
  main,
  test,
  run,
  run2,
)
where

import Elea.Prelude
import Elea.Tests.All ( runTests )
import Elea.Show ( showM )
import Text.Printf
import System.CPUTime
import qualified Elea.Testing as Test
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
    
test2 :: IO ()
test2 = time runTests

main = test

test :: IO ()
test = time
  $ Test.execute
  . Fedd.eval $ do
    Test.loadPrelude
    phd_props <- Test.loadFile "phd_tests.elea"
    tests <- mapM Test.checkProp phd_props 
    return (Test.list tests)

run :: String -> IO ()
run term_def = id
  . time
  $ putStrLn
  . Fedd.eval $ do
    Test.loadPrelude
    term <- Test.term term_def
    term' <- Fusion.run term
    term_s <- showM term'
    return term_s
    
run2 :: String -> IO ()
run2 term_def = id
  . putStrLn
  . Fedd.eval $ do
    Test.loadPrelude
    term <- Test.term term_def
    term' <- Fusion.run term
    term'' <- Fusion.run term'
    term_s <- showM term''
    return term_s
  
  
    
