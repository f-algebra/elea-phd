module Elea.Testing (
  Test, execute,
  label, list, run, assert
) where

import Prelude ()
import Elea.Prelude hiding ( assert )
import Elea.Core ( Elea )
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
run = HUnit.test . flip evalState mempty

assert :: HUnit.Assertable t => t -> Test
assert = HUnit.TestCase . HUnit.assert



{-
prelude :: String
prelude = unsafePerformIO
  $ readFile "prelude.zthy"
  
run :: HUnit.Testable t => Zeno t -> Test
run = HUnit.test . flip evalState empty

term :: String -> Zeno ZTerm
term = ZML.readTerm

assertAlphaEq :: ZTerm -> ZTerm -> Test
assertAlphaEq x y = 
  label (show (Logic.Equal x y))
  $ assert (x `alphaEq` y)

newVar :: String -> String -> Zeno ZVar
newVar name_s typ_s = do
  typ <- ZML.readType typ_s
  new_var <- Var.declare name_s typ Var.Universal
  Zeno.defineTerm name_s (Term.Var new_var)
  return new_var
  
-- Move this into the counter example finding module
-- it's a assertion that will dynamically check equality
assertEq :: ZTerm -> ZTerm -> a -> a
assertEq t1 t2 x = x
 
loadPrelude :: Zeno ()
loadPrelude =
  mapM_ process 
    $ ZML.readLines prelude
  where
  process ("let", arg) = ZML.readBinding arg
  process ("type", arg) = ZML.readTypeDef arg
 -}

