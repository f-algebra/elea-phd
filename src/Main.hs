module Main
(
  module Elea.Prelude,
  main,
  test,
  apply,
  dec
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
import qualified Data.Poset as Quasi
import qualified Test.Framework as TestFramework
import qualified Test.Framework.Providers.HUnit as TestFramework

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
  args <- getArgs
  case head args of
      "test" -> do
        tests <- liftM TestFramework.hUnitTestToTests Tests.all
        TestFramework.defaultMainWithArgs tests (tail args)

test :: IO ()
test = withArgs ["test"] main

applyM :: String -> Test.M String
applyM term_def = do
  Fedd.loadPrelude
  term <- Test.term term_def
  (term', steps_taken) <- Steps.listen (Fusion.applyM term)
  return 
    $ printf "\n%s\n\n:%s\n\nin %d steps" term' (Type.get term') steps_taken
    
apply :: String -> IO ()
apply term_def = time $ do
  result <- Fedd.evalT (applyM term_def)
  putStrLn result

dec :: String -> IO ()
dec term_def = time $ do
  result <- id
    . Fedd.evalT 
    . Direction.local Direction.Dec
    $ applyM term_def
  putStrLn result
