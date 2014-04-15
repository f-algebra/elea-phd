module Elea.Testing 
(
  Test, M, execute,
  label, list, run, 
  assert, assertEq, assertNot,
  assertSimpEq,
  loadPrelude, loadFile,
  term, _type,
  simplifiedTerm,
  fusedTerm,
  assertProvablyEq,
  assertTermEq,
  localVars,
) 
where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Term
import Elea.Type
import Elea.Show
import Elea.Monad.Fedd ( Fedd )
-- import qualified Elea.Parser.Haskell as Haskell
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.Env as Env
import qualified Elea.Parser.Calculus as Parse
import qualified Elea.Simplifier as Simp
import qualified Elea.Equality as Equality
import qualified Elea.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Discovery as Discovery
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Fusion.Class as Fusion
import qualified Test.HUnit as HUnit

type Test = HUnit.Test
type M = Fedd

execute :: Test -> IO ()
execute test = do
  HUnit.runTestTT test
  return ()

list :: [Test] -> Test
list = HUnit.TestList

label :: String -> Test -> Test
label = HUnit.TestLabel

run :: HUnit.Testable t => Fedd t -> Test
run = HUnit.test . Fedd.eval . Discovery.trace

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
  
assertProvablyEq :: Fusion.FusionM m => Term -> Term -> m Test
assertProvablyEq t1 t2 = do
  mby_eq <- runMaybeT (Equality.prove Fusion.run t1 t2)
  t1s <- showM t1
  t2s <- showM t2
  let prop_s = "\nexpected: " ++ t1s ++ "\nbut got: " ++ t2s
  return 
    . HUnit.TestCase   
    . HUnit.assertBool prop_s
    $ fromMaybe False mby_eq
  
assertTermEq :: Fusion.FusionM m => Term -> Term -> m Test
assertTermEq t1 t2 = do
  t1s <- showM t1
  t2s <- showM t2
  let prop_s = "\nexpected: " ++ t1s ++ "\nbut got: " ++ t2s
  return 
    . HUnit.TestCase
    . HUnit.assertBool prop_s 
    $ t1 == t2

term :: (Defs.Has m, Env.Read m) => String -> m Term
term = Err.noneM . Parse.term

simplifiedTerm :: (Defs.Has m, Env.Read m) => String -> m Term
simplifiedTerm = liftM Simp.run . term

fusedTerm :: (Defs.Has m, Fusion.FusionM m) => String -> m Term
fusedTerm = Fusion.run <=< term

_type :: Defs.Has m => String -> m Type
_type = Err.noneM . Parse._type

localVars :: (Defs.Has m, Env.Write m) => String -> m a -> m a
localVars bs_s run = do
  bs <- Err.noneM (Parse.bindings bs_s)
  Env.bindMany bs $ do
    zipWithM defineBind [0..length bs - 1] (reverse bs)
    run
  where
  defineBind idx (Bind lbl _) =
    Defs.defineTerm lbl p_term
    where
    p_term = polymorphic [] (const (Var (enum idx)))
