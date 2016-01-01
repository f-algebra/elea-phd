module Elea.Testing 
(
  M,
  test, testWithPrelude,
  assertSimpEq,
  label, list,
  loadPrelude, loadFile,
  loadPropertyNamesFromFile,
  term, _type,
  simplifiedTerm,
  assertTermEq,
  assertBool,
  assertEq,
  localVars,
  module Test.HUnit
) 
where

import Elea.Prelude
import Elea.Term
import Elea.Type hiding ( get )
import Elea.Show
import Elea.Monad.Fedd ( FeddT )
import Test.HUnit ( Test, assertFailure )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.Env as Env
import qualified Elea.Parser.Calculus as Parse
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Discovery as Discovery
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Memo.Class as Memo
import qualified Test.HUnit as HUnit
import qualified Control.Monad.Trans.Class as Trans

type M = FeddT IO

test :: String -> M () -> Test
test label = id
  . HUnit.TestLabel label 
  . HUnit.TestCase 
  . Fedd.evalT

label :: String -> Test -> Test
label = HUnit.TestLabel

list :: [Test] -> Test
list = HUnit.TestList

testWithPrelude :: String -> M () -> Test
testWithPrelude label = test label . (loadPrelude >>)

assertBool :: String -> Bool -> M ()
assertBool msg = Trans.lift . HUnit.assertBool msg

assertEq :: (Show a, Eq a) => String -> a -> a -> M ()
assertEq msg x y = Trans.lift (HUnit.assertEqual msg x y)

assertTermEq :: String ->Term -> Term -> M ()
assertTermEq msg (stripTags -> t1) (stripTags -> t2) = do
  t1s <- showM t1
  t2s <- showM t2
  let msg' | null msg = ""
           | otherwise = printf "[%s]\n" msg
  let prop_s = printf "%sexpected: %s\nbut got: %s" msg' t1s t2s
  assertEq prop_s t1 t2

assertSimpEq :: String -> Term -> Term -> M ()
assertSimpEq msg t1 t2 = do
  t1' <- Simp.runM t1
  t2' <- Simp.runM t2
  assertTermEq msg t1 t2

loadFile :: String -> M [Prop]
loadFile file_name = do
  contents <- Trans.lift (readFile file_name) 
  Err.noneM 
    . liftM (map uninterpreted) 
    $ Parse.program contents

loadPropertyNamesFromFile :: String -> IO [String]
loadPropertyNamesFromFile file_name = do
  all_props <- Fedd.evalT $ do
    loadPrelude
    loadFile file_name
  return (map (get propName) all_props)

loadPrelude :: M ()
loadPrelude = do
  [] <- loadFile "prelude.elea"
  return ()
    
term :: (Tag.Gen m, Defs.Has m, Env.Read m) => String -> m Term
term = Err.noneM . Parse.term

simplifiedTerm :: (Tag.Gen m, Defs.Has m, Env.Read m) => String -> m Term
simplifiedTerm = liftM Simp.run . term

_type :: (Tag.Gen m, Defs.Has m) => String -> m Type
_type = Err.noneM . Parse._type

localVars :: String -> M a -> M a
localVars bs_s run = do
  bs <- Err.noneM (Parse.bindings bs_s)
  Env.bindMany bs $ do
    zipWithM defineBind [0..] (reverse bs)
    run
  where
  defineBind idx (Bind lbl _) =
    Defs.defineTerm lbl p_term
    where
    p_term = polymorphic [] (const (Var (enum idx)))
