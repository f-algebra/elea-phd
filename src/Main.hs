module Main
(
  module Elea.Prelude,
  main,
  test,
)
where

import Elea.Prelude
import Elea.Show ( showM )
import Text.Printf
import System.CPUTime
import System.Environment ( getArgs, withArgs )

import qualified Elea.Testing as Test
import qualified Elea.Tests.All as Tests
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
--import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Direction as Direction
--import qualified Elea.Monad.Fedd as Fedd
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

    {-
runM :: String -> Test.M String
runM term_def = do
  Test.loadPrelude
  term <- Test.term term_def
  (term', steps_taken) <- Steps.listen (Fusion.run term)
  term_s <- showM term'
  ty_s <- liftM show (Type.getM term')
  return ("\n" ++ term_s ++ "\n\n: " ++ ty_s ++ "\n\n in " ++ show steps_taken ++ " steps")
    
run :: String -> IO ()
run term_def = time $ do
  result <- Fedd.evalT (runM term_def)
  putStrLn result

dec :: String -> IO ()
dec term_def = time $ do
  result <- id
    . Fedd.evalT 
    . Direction.local Direction.Dec
    $ runM term_def
  putStrLn result
-}
