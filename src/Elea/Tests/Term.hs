module Elea.Tests.Term
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Term.Ext
import Elea.Type
import Elea.Testing ( Test )
import Elea.Monad.Fedd.Include ()
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

tests = id
  . Test.label "Term" 
  . Test.list 
  $ [ testBuildFold, testConjunction, testSubterms, testAbstract
    , testFindArgs, testRecursiveId, testEquateArgs, testStrictWithin ]

testBuildFold :: Test
testBuildFold = 
  Test.testWithPrelude "build fold" $ do
    Test.assertSimpEq "fold: nat -> nat" 
      fold_nat_nat (buildFold nat (Base nat))
    Test.assertSimpEq "fold: tree<nat> -> list<nat>" 
      fold_ntree_nlist (buildFold ntree (Base nlist))
  where
  Base nat = read "nat"
  Base ntree = read "tree<nat>"
  Base nlist = read "list<nat>"

  fold_nat_nat = read
    $ "fun (v: nat) (k: nat -> nat) -> "
    ++  "fix (f: nat -> nat) (x: nat) -> "
    ++  "match x with | 0 -> v | Suc x' -> k (f x') end"

  fold_ntree_nlist = read
    $ "fun (v: list<nat>) (k: list<nat> -> nat -> list<nat> -> list<nat>) -> "
    ++  "fix (f: tree<nat> -> list<nat>) (t: tree<nat>) -> "
    ++  "match t with | Leaf -> v | Node t1 x t2 -> k (f t1) x (f t2) end"

testConjunction :: Test
testConjunction = 
  Test.test "conjunction" $ do
    Test.assertSimpEq "conjunction" 
      (read "fun (p q r: bool) -> and p (and q r)")
      (conjunction 3)
      
testSubterms :: Test
testSubterms = Test.test "subterms" $ do     
  Test.assertEq "free subterms" (Set.fromList [add_x_y]) (Set.fromList free_ts) 
  Test.assertEq "free vars 1" (Set.fromList [x, y]) free_vars
  Test.assertEq "free vars 2" free_vars free_vars2
  Test.assertEq "remove subterms" [add_stuff, two] removed_ts 
  where
  [x, y, one, two, add_x_y, add_stuff] = 
    Test.withEnv "(x y: nat)"
    ["x", "y", "1", "2", "add x y", "add (add x y) 1"]

  free_ts = freeSubtermsOf add_stuff
  free_vars = freeVarSet add_stuff
  free_vars2 = freeVarSet add_x_y
  removed_ts = removeSubterms [add_stuff, one, two, y]

testAbstract :: Test
testAbstract = Test.test "abstract" $ do
  Test.assertTermEq "abstract" abs_xy abs_xy'
  where
  [xy, x, y, abs_xy] = Test.withEnv "(x y: nat)" 
    ["add x y", "x", "y", "fun (x y: nat) -> add y x"]
  abs_xy' = abstractVars [y, x] xy
    
testFindArgs :: Test
testFindArgs = Test.test "find args" $ do
  Test.assertTermEq "findArgs" ctx_arg arg
  where
  [ctx_t, in_ctx, ctx_arg] = 
    Test.withEnv "(f: list<nat> -> list<nat>) (xs: list<nat>) (n x: nat)"
    [ "fun (ys: list<nat>) -> Cons<nat> n ys"
    , "Cons<nat> n (append<nat> (f xs) (Cons<nat> x Nil<nat>))"
    , "append<nat> (f xs) (Cons<nat> x Nil<nat>)" ]

  Just [arg] = findArguments ctx_t in_ctx
    
testRecursiveId :: Test
testRecursiveId = Test.test "recursive id" $ do
  Test.assertTermEq "eval" (read def_nat_id) (Eval.apply (recursiveId nat))
  where
  Base nat = read "nat"

testEquateArgs :: Test
testEquateArgs = Test.test "equateArgs" $ do
  let t2' = equateArgs 0 2 t1
  Test.assertEq "equate 0 2" t2 t2' 
  
  let t3' = equateArgs 1 2 t2
  Test.assertEq "equate 1 2" t3 t3'
      
  let t3'' = equateArgsMany [(0, 2), (1, 3)] t1
  Test.assertEq "equate many" t3 t3''
  where
  [t1, t2, t3] = Test.withEnv "(f: nat -> nat -> nat -> nat -> nat)"
    [ "fun (a b c d: nat) -> f a b c d"
    , "fun (a b d: nat) -> f a b a d"
    , "fun (a b: nat) -> f a b a b" ]


testStrictWithin :: Test
testStrictWithin = Test.test "strictWithin" $ do
  let strict1' = strictWithin t1
      strict1 = Set.fromList [x, y]
      
  let strict2' = strictWithin t2
      strict2 = strict1
  Test.assertEq "strict within list" strict1 strict1' 
  Test.assertEq "strict in term" strict2 strict2'
  where
  [t1, x, y, t2] = Test.withEnv "(x y z: nat)"
    [ def_strict_test, "x", "y", "not (eq x y)" ]


def_eq_unit, def_eq_bool, def_eq, def_eq_ntree :: String
def_eq_unit = 
  "fun (u v: unit) -> True"
def_eq_bool =
  "fun (p q: bool) -> if p then q else not q"
def_eq =
  "fix (eq: nat -> nat -> bool) (x y: nat) -> "
  ++ "match x with "
  ++ "| 0 -> match y with | 0 -> True | Suc y' -> False end "
  ++ "| Suc x' -> match y with | 0 -> False | Suc y' -> eq x' y' end "
  ++ "end"
def_eq_ntree =
  "fix (eq: tree<nat> -> tree<nat> -> bool) (t t': tree<nat>) -> "
  ++ "match t with "
  ++ "| Leaf -> match t' with | Leaf -> True | Node t1' x' t2' -> False end "
  ++ "| Node t1 x t2 -> match t' with "
    ++ "| Leaf -> False "
    ++ "| Node t1' x' t2' -> and (eq t1 t1') (and (eq[nat] x x') (eq t2 t2')) "
    ++ "end end"
    
def_add_raw =
  "fix (add: nat -> nat -> nat) (y: nat) (x: nat) -> "
  ++ "match x with | 0 -> y | Suc x' -> Suc (add y x') end"
  
def_nat_id = 
  "fix (id: nat -> nat) (x: nat) -> "
  ++ "match x with | 0 -> 0 | Suc x' -> Suc (id x') end"
  
def_strict_test = 
  "match x with "
  ++ "| 0 -> match y with | 0 -> False | Suc y' -> eq z z end "
  ++ "| Suc x' -> eq y z end"
    
