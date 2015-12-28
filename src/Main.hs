module Main
(
  module Elea.Prelude,
  main,
  test,
  run,
  dec,
  embeds
)
where

import Elea.Prelude
import Elea.Show ( showM )
import Text.Printf
import System.CPUTime
import System.Environment ( getArgs )

import qualified Elea.Testing as Test
import qualified Test.Framework as Test
import qualified Elea.Tests.All as Tests
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Fedd as Fedd
import qualified Data.Poset as Quasi

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10**12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v
    
main :: IO ()
main = do
  args <- getArgs
  case args !! 0 of
      "test" -> Test.defaultMainWithArgs Tests.all (tail args)

test :: IO ()
test = time
  $ Test.execute
  . Fedd.eval $ do
    Test.loadPrelude
    phd_props <- Test.loadFile "phd_tests.elea"
    tests <- mapM Test.checkProp phd_props 
    return (Test.list tests)

runM :: String -> Fedd.Fedd String
runM term_def = do
  Test.loadPrelude
  term <- Test.term term_def
  term' <- Fusion.run term
  term_s <- showM term'
  ty_s <- liftM show (Type.getM term')
  return ("\n" ++ term_s ++ "\n\n: " ++ ty_s)
    
run :: String -> IO ()
run = id
  . time
  . putStrLn
  . Fedd.eval 
  . runM
  
dec :: String -> IO ()
dec = id
  . time
  . putStrLn
  . Fedd.eval 
  . Direction.local Direction.Dec
  . runM

embeds :: String -> String -> Bool
embeds t1_def t2_def = id
  . Fedd.eval $ do
    Test.loadPrelude 
    t1 <- Test.term t1_def
    t2 <- Test.term t2_def 
    return (t1 Quasi.<= t2)
  
  
    
