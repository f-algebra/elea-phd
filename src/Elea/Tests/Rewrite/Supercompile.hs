module Elea.Tests.Rewrite.Supercompile (
  tests
) where

import Elea.Prelude
import Elea.Term
import Elea.Testing ( Test )
import qualified Elea.Testing as Test
import qualified Elea.Rewrite.Supercompile as Supercompiler

tests :: IO Test
tests = return
  . Test.label "Supercompiler"
  . Test.list
  $ [ testFindUnfolding ]


testFindUnfolding :: Test 
testFindUnfolding = Test.test "find unfolding" $ do
  Test.assertEq "context for add (add x y) z" add_ctx add_ctx'
  Test.assertEq "unfold term for add (add x y) z" add_xy add_xy'

  Test.assertEq "context for zeno3" le_count_ctx le_count_ctx'
  Test.assertEq "unfold term 1 for zeno3" append_xs_ys append_xs_ys'
  Test.assertEq "unfold term 2 for zeno3" count_xs count_xs'
  where
  [add_xyz, add_xy, add_ctx] = Test.withEnv "(x y z: nat)"
    [ "add (add x y) z", "add x y", "fun (n: nat) -> add n z" ]

  (add_ctx', [add_xy']) = Supercompiler.findUnfolding add_xyz

  [le_count_app, count_xs, append_xs_ys, le_count_ctx] = 
    Test.withEnv "(n: nat) (xs ys: list<nat>)"
    [ "le (count n xs) (count n (append<nat> xs ys))"
    , "count n xs"
    , "append<nat> xs ys" 
    , "fun (zs: list<nat>) (k: nat) -> le k (count n zs)" ]

  (le_count_ctx', [append_xs_ys', count_xs']) = Supercompiler.findUnfolding le_count_app
