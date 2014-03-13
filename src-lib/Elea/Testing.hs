module Elea.Testing 
(
  Test, M, execute,
  label, list, run, 
  assert, assertEq, assertNot,
  assertSimpEq,
  loadPrelude, loadFile,
  term, _type,
  simplifiedTerm,
  assertProvablyEq,
  assertTermEq,
) 
where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Term
import Elea.Type
import Elea.Monad.Elea ( Elea )
import Elea.Show
import qualified Elea.Env as Env
import qualified Elea.Parser as Parse
import qualified Elea.Simplifier as Simp
import qualified Elea.Equality as Equality
import qualified Elea.Fusion as Fusion
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

assertSimpEq :: Term -> Term -> Test
assertSimpEq (Simp.run -> t1) (Simp.run -> t2) = 
  assertEq t1 t2

prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.elea"
  
loadFile :: Defs.Has m => String -> m [Equation]
loadFile = id
  . Err.noneM 
  . liftM (map uninterpreted) 
  . Parse.program 
  . unsafePerformIO 
  . readFile

loadPrelude :: Defs.Has m => m ()
loadPrelude = do
  eqs <- Err.noneM (Parse.program prelude)
  return ()
  
assertProvablyEq :: (Defs.Has m, Env.Read m) => Term -> Term -> m Test
assertProvablyEq t1 t2 = do
  mby_eq <- runMaybeT (Equality.prove Fusion.run t1 t2)
  t1s <- showM t1
  t2s <- showM t2
  let prop_s = "\nexpected: " ++ t1s ++ "\nbut got: " ++ t2s
  return 
    . HUnit.TestCase   
    . HUnit.assertBool prop_s
    $ fromMaybe False mby_eq
  
assertTermEq :: (Defs.Has m, Env.Read m) => Term -> Term -> m Test
assertTermEq t1 t2 = do
  t1s <- showM t1
  t2s <- showM t2
  let prop_s = "\nexpected: " ++ t1s ++ "\nbut got: " ++ t2s
  return 
    . HUnit.TestCase
    . HUnit.assertBool prop_s 
    $ t1 == t2

term :: Defs.Has m => String -> m Term
term = Err.noneM . Parse.term

simplifiedTerm :: Defs.Has m => String -> m Term
simplifiedTerm = liftM Simp.run . term

_type :: Defs.Has m => String -> m Type
_type = Err.noneM . Parse._type

