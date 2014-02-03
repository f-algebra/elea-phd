module Elea.Testing 
(
  Test, M, execute,
  label, list, run, 
  assert, assertEq, assertNot,
  assertSimpEq,
  loadPrelude, loadFile,
  term, _type,
) 
where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Term
import Elea.Monad.Elea ( Elea )
import qualified Elea.Env as Env
import qualified Elea.Parser as Parse
import qualified Elea.Simplifier as Simp
import qualified Elea.Definitions as Defs
import qualified Elea.Monad.Error as Err
import qualified Test.HUnit as HUnit

type Test = HUnit.Test
type M = Defs.DBStateT (Reader [Bind])

execute :: Test -> IO ()
execute test = do
  HUnit.runTestTT test
  return ()

list :: [Test] -> Test
list = HUnit.TestList

label :: String -> Test -> Test
label = HUnit.TestLabel

run :: HUnit.Testable t => M t -> Test
run = HUnit.test . Env.empty . Defs.evalEmptyT 

assert :: HUnit.Assertable t => t -> Test
assert = HUnit.TestCase . HUnit.assert

assertNot :: Bool -> Test
assertNot = assert . not

assertEq :: (Show a, Eq a) => a -> a -> Test
assertEq = (HUnit.TestCase .) . HUnit.assertEqual ""

assertSimpEq :: (Defs.Read m, Env.Read m) => Term -> Term -> m Test
assertSimpEq t1 t2 = do
  t1' <- Simp.run t1
  t2' <- Simp.run t2
  return (assertEq t1' t2')

prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.elea"
  
loadFile :: Defs.Has m => String -> m [Equation]
loadFile = Err.noneM . Parse.program . unsafePerformIO . readFile

loadPrelude :: Defs.Has m => m ()
loadPrelude = do
  eqs <- Err.noneM (Parse.program prelude)
  return ()

term :: Defs.Has m => String -> m Term
term = Err.noneM . Parse.term

_type :: Defs.Has m => String -> m Type
_type = Err.noneM . Parse._type

