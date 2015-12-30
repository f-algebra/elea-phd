module Elea.Testing 
(
  M, Test,
  test, testWithPrelude,
  assertSimpEq,
  label, list,
  loadPrelude, loadFile,
  term, _type,
  simplifiedTerm,
  assertProp,
  assertTermEq,
  localVars,
  module Test.HUnit
) 
where

import Elea.Prelude
import Elea.Term
import Elea.Type
import Elea.Show
import Elea.Monad.Fedd ( FeddT )
import Test.HUnit ( Test, assertEqual, assertBool, assertFailure )
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

assertTermEq :: Term -> Term -> M ()
assertTermEq (stripTags -> t1) (stripTags -> t2) = do
  t1s <- showM t1
  t2s <- showM t2
  let prop_s = printf "expected: %s\nbut got: %s" t1s t2s
  Trans.lift
    $ assertEqual prop_s t1 t2

assertSimpEq :: Term -> Term -> M ()
assertSimpEq t1 t2 = do
  t1' <- Simp.runM t1
  t2' <- Simp.runM t2
  assertTermEq t1 t2
      
assertProp :: Prop -> M ()
assertProp (Prop name prop_t) = do
  prop_t' <- Fusion.run prop_t
  if isBot prop_t'
  then return ()
  else do
    prop_s <- showM prop_t'
    Trans.lift
      . assertFailure
      $ printf "property \"%s\" failed with: %s" name prop_s

loadFile :: String -> M [Prop]
loadFile file_name = do
  contents <- Trans.lift (readFile file_name) 
  Err.noneM 
    . liftM (map uninterpreted) 
    $ Parse.program contents

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

localVars :: (Tag.Gen m, Defs.Has m, Env.Write m) => String -> m a -> m a
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
