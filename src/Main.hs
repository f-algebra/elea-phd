module Main
(
  module Elea.Prelude,
  main,
  test,
  apply,
  dec,
  drive
)
where

import Elea.Prelude hiding ( run )
import Text.Printf
import System.CPUTime
import System.Environment ( getArgs, withArgs )

import qualified Elea.Testing as Test
import qualified Elea.Monad.Fedd.Include as Fedd
import qualified Elea.Tests.All as Tests
import qualified Elea.Term.Ext as Term
import qualified Elea.Type as Type
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.StepCounter as Steps
import qualified Elea.Monad.Transform.TraceSteps as TraceSteps
import qualified Elea.Rewrite.Drive as Drive
import qualified Data.Poset as Quasi
import qualified Test.Framework as TestFramework
import qualified Test.Framework.Providers.HUnit as TestFramework
import qualified Control.Exception as Control


time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10**12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
    
main :: IO ()
main = do
  Control.finally runTests Test.printTestTimes
  where
  runTests = do
    args <- getArgs
    case head args of
        "test" -> do
          tests <- liftM TestFramework.hUnitTestToTests Tests.all
          TestFramework.defaultMainWithArgs tests (tail args)
        "inc" -> do
           apply (args !! 1)

  printMessages = do
    putStrLn "Test runtimes"


test :: IO ()
test = withArgs ["test"] main

applyM :: String -> Test.M String
applyM (read -> term) = do
  term' <- TraceSteps.enable (Fusion.applyM term)
  return 
    $ printf "\n%s\n\n:%s" term' (Type.get term')
    
apply :: String -> IO ()
apply term_def = do
  result <- Fedd.evalT (applyM term_def)
  putStrLn result

dec :: String -> IO ()
dec term_def = time $ do
  result <- id
    . Fedd.evalT 
    . Direction.local Direction.Dec
    $ applyM term_def
  putStrLn result

drive :: String -> IO ()
drive = id
  . putStrLn 
  . show
  . Drive.inc
  . read
