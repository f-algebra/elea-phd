module Elea.Testing (
  Test, M, execute,
  label, list, run, 
  assert, assertEq, assertNot,
  loadPrelude, loadFile,
  term
) where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )

import qualified Elea.Parser as Parse
import qualified Elea.Typing as Typing
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Test.HUnit as HUnit

type Test = HUnit.Test
type M a = State (Map String Term) a

execute :: Test -> IO ()
execute test = do
  HUnit.runTestTT test
  return ()

list :: [Test] -> Test
list = HUnit.TestList

label :: String -> Test -> Test
label = HUnit.TestLabel

run :: HUnit.Testable t => M t -> Test
run = HUnit.test . flip evalState mempty 

assert :: HUnit.Assertable t => t -> Test
assert = HUnit.TestCase . HUnit.assert

assertNot :: Bool -> Test
assertNot = assert . not

assertEq :: (Show a, Eq a) => a -> a -> Test
assertEq = (HUnit.TestCase .) . HUnit.assertEqual ""

prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.elea"
  
loadFile :: Defs.Monad m => String -> m ()
loadFile = Err.noneM . Parse.program . unsafePerformIO . readFile

loadPrelude :: Defs.Monad m => m ()
loadPrelude = Err.noneM (Parse.program prelude)

term :: Defs.Monad m => String -> m Term
term = Err.noneM . Parse.term

