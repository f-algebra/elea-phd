module Elea.Tests.Rewrite.Supercompile (
  tests
) where

import Elea.Prelude
import Elea.Term
import Elea.Testing ( Test )
import Elea.Monad.Direction ( Direction )
import qualified Elea.Testing as Test
import qualified Elea.Rewrite.Supercompile as Supercompiler

tests :: IO Test
tests = return
  . Test.label "Supercompiler"
  . Test.list
  $ [ testNextAction ]


testNextAction :: Test 
testNextAction = Test.test "next action" $ do
  Test.assertEq "rev (rev xs)" rev_rev_action rev_rev_action'
  Test.assertEq "minus (add n (Suc m)) (Suc m)" minus_add_action minus_add_action'
  where
  getNextAction :: Term -> Supercompiler.Action
  getNextAction = id
    . (run :: Reader Direction a -> a) 
    . Supercompiler.nextAction

  rev_rev_action = Supercompiler.Unfold rev
  rev_rev_action' = getNextAction rev_rev

  minus_add_action = Supercompiler.Induction n
  minus_add_action' = getNextAction minus_add

  [ rev_rev, rev, n, minus_add ] = Test.withEnv "(xs: list<nat>) (n m: nat)"
    [ "rev<nat> (rev<nat> xs)"
    , "rev<nat> xs"
    , "n"
    , "minus (add n (Suc m)) (Suc m)" ]
