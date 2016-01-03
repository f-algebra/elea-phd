module Elea.Tests.Term
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Term.Ext
import Elea.Type
import Elea.Show
import Elea.Testing ( Test )
import qualified Elea.Type as Type
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
    , testFindArgs, testEval, testEquateArgs, testStrictWithin ]

testBuildFold :: Test
testBuildFold = 
  Test.testWithPrelude "buildFold" $ do
  fold_nat_nat <- Test.term 
    $ "fun (v: nat) (k: nat -> nat) -> "
    ++  "fix (f: nat -> nat) (x: nat) ->"
    ++  "match x with | 0 -> v | Suc x' -> k (f x') end"
    
  fold_ntree_nlist <- Test.term
    $ "fun (v: list<nat>) (k: list<nat> -> nat -> list<nat> -> list<nat>) -> "
    ++  "fix (f: tree<nat> -> list<nat>) (t: tree<nat>) -> "
    ++  "match t with | Leaf -> v | Node t1 x t2 -> k (f t1) x (f t2) end"
  
  Base nat <- Test._type "nat"
  Base ntree <- Test._type "tree<nat>"
  Base nlist <- Test._type "list<nat>"
  
  Test.assertSimpEq "fold: nat -> nat" 
    fold_nat_nat (buildFold nat (Base nat))
  Test.assertSimpEq "fold: tree<nat> -> list<nat>" 
    fold_ntree_nlist (buildFold ntree (Base nlist))
      
testConjunction :: Test
testConjunction = 
  Test.testWithPrelude "conjunction" $ do
    let conj3_t = Simp.run (conjunction 3)
    conj3_t' <- Test.simplifiedTerm "fun (p q r: bool) -> and p (and q r)"
    Test.assertEq "conjunction" conj3_t' conj3_t
      
testSubterms :: Test
testSubterms = id
  . Test.testWithPrelude "subterms"
  . Test.localVars "(x y: nat)" $ do
      add_stuff <- Test.term "add (add x y) 1"
      x <- Test.term "x"
      one <- Test.term "1"
      two <- Test.term "2"
      y <- Test.term "y"
      add_x_y <- Test.term "add x y"
      
      let free_ts = freeSubtermsOf add_stuff
          free_vars = freeVars add_stuff
          free_vars2 = freeVars add_x_y
          removed_ts = removeSubterms [add_stuff, one, two, y]
      Test.assertEq "free subterms" (Set.fromList [x]) free_ts 
      Test.assertEq "free vars 1" (Set.fromList [x, y]) free_vars
      Test.assertEq "free vars 2" free_vars free_vars2
      Test.assertEq "remove subterms" [add_stuff, two] removed_ts 
    
testAbstract :: Test
testAbstract = id
  . Test.testWithPrelude "abstract"
  . Test.localVars "(x y: nat)" $ do
      xy <- Test.term "add x y"
      x <- Test.term "x"
      y <- Test.term "y"
      abs_xy <- Test.term "fun (x y: nat) -> add y x"
      let abs_bs = fst (flattenLam abs_xy)
          abs_xy' = abstractVars [y, x] xy
      Test.assertTermEq "abstract" abs_xy abs_xy'
    
testFindArgs :: Test
testFindArgs = id
  . Test.testWithPrelude "findArgs" 
  . Test.localVars "(f: list<nat> -> list<nat>) (xs: list<nat>) (n x: nat)" $ do
      ctx_t <- Test.term "fun (ys: list<nat>) -> Cons<nat> n ys"
      in_ctx <- Test.term "Cons<nat> n (append<nat> (f xs) (Cons<nat> x Nil<nat>))"
      ctx_arg <- Test.term "append<nat> (f xs) (Cons<nat> x Nil<nat>)"
      Just [arg] <- runMaybeT (findArguments ctx_t in_ctx)
      Test.assertTermEq "findArgs" ctx_arg arg
    
testEval :: Test
testEval = id
  . Test.testWithPrelude "eval" $ do
      Base nat <- Test._type "nat"
      let id_nat = Eval.run (recursiveId nat)
      id_nat' <- Test.term def_nat_id
      Test.assertTermEq "eval" id_nat' id_nat
      
testEquateArgs :: Test
testEquateArgs = id
  . Test.testWithPrelude "equateArgs"
  . Test.localVars "(f: nat -> nat -> nat -> nat -> nat)" $ do
      t1 <- Test.term "fun (a b c d: nat) -> f a b c d"
      t2 <- Test.term "fun (a b d: nat) -> f a b a d"
      let t2' = equateArgs 0 2 t1
      Test.assertEq "equate 0 2" t2 t2' 
      
      t3 <- Test.term "fun (a b: nat) -> f a b a b"
      let t3' = equateArgs 1 2 t2
      Test.assertEq "equate 1 2" t3 t3'
          
      let t3'' = equateArgsMany [(0, 2), (1, 3)] t1
      Test.assertEq "equate many" t3 t3''

testStrictWithin :: Test
testStrictWithin = id
  . Test.testWithPrelude "strictWithin"
  . Test.localVars "(x y z: nat)" $ do
      t1 <- Test.term def_strict_test
      x <- Test.term "x"
      y <- Test.term "y"
      let strict1' = strictWithin t1
          strict1 = Set.fromList [x, y]
          
      t2 <- Test.term "not (eq x y)"
      let strict2' = strictWithin t2
          strict2 = strict1
          
      Test.assertEq "strict within list" strict1 strict1' 
      Test.assertEq "strict in term" strict2 strict2'
      
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
    
