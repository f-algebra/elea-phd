module Elea.Testing (
  Test, execute,
  label, list, run, 
  assert, assertEq, 
  loadPrelude, loadFile,
  term
) where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Term ( Term )
import Elea.Monad.Elea ( Elea )

import qualified Elea.Parser as Parse
import qualified Elea.Typing as Typing
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Error
import qualified Test.HUnit as HUnit

type Test = HUnit.Test

execute :: Test -> IO ()
execute test = do
  HUnit.runTestTT test
  return ()

list :: [Test] -> Test
list = HUnit.TestList

label :: String -> Test -> Test
label = HUnit.TestLabel

run :: HUnit.Testable t => Elea t -> Test
run = HUnit.test . Elea.unsafe

assert :: HUnit.Assertable t => t -> Test
assert = HUnit.TestCase . HUnit.assert

assertEq :: (Show a, Eq a) => a -> a -> Test
assertEq = (HUnit.TestCase .) . HUnit.assertEqual ""

prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.elea"
  
loadFile :: String -> Elea ()
loadFile = Parse.program . unsafePerformIO . readFile

loadPrelude :: Elea ()
loadPrelude = Parse.program prelude

term :: String -> Elea Term
term = Error.check Typing.check . Parse.term
