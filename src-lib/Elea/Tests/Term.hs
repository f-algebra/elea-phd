module Elea.Tests.Term
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Terms
import Elea.Type
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Evaluation as Eval
import qualified Elea.Simplifier as Simp
import qualified Elea.Fusion as Fusion
import qualified Elea.Definitions as Defs
import qualified Data.Set as Set

tests = Test.label "Terms"
    $ Test.run $ do
  Test.loadPrelude
  
  add_fix <- Test.term "add"
  
  leq_nat <- Test.term "leq_nat"
  srtd_fix <- Test.term "is_sorted"
  
  one <- Test.term "1"
  Lam _ x_list <- Test.term "fun (x: nat) -> Cons x Nil"
  xs_list <- liftM (\t -> app t [Var 0]) (Test.term "Cons 1")
   
  let dec1 = Test.assertEq [0] (decreasingArgs add_fix)
      dec2 = Test.assertEq [0, 1] (decreasingArgs leq_nat)
      dec3 = Test.assertEq [0] (decreasingArgs srtd_fix)
      
      fin1 = Test.assert (isFinite one)
      fin2 = Test.assert (isFinite x_list)
      fin3 = Test.assertNot (isFinite xs_list)
      
  fold_nat_nat <- Test.term 
    $ "fun (v: nat) (k: nat -> nat) -> "
    ++  "fix (f: nat -> nat) (x: nat) ->"
    ++  "match x with | 0 -> v | Suc x' -> k (f x') end"
    
  fold_ntree_nlist <- Test.term
    $ "fun (v: nlist) (k: nlist -> nat -> nlist -> nlist) -> "
    ++  "fix (f: ntree -> nlist) (t: ntree) -> "
    ++  "match t with | Leaf -> v | Node t1 x t2 -> k (f t1) x (f t2) end"
  
  Base nat <- Test._type "nat"
  Base ntree <- Test._type "ntree"
  Base nlist <- Test._type "nlist"
  
  let fold1 = Test.assertEq fold_nat_nat (buildFold nat (Base nat))
      fold2 = Test.assertEq fold_ntree_nlist (buildFold ntree (Base nlist))
      
  -- A weird problem I had at one point
  let eq_ind = Ind "__EQ" [("==", [ConArg (Base nat), ConArg (Base nat)])]
      weird_free = Indices.free (Alt [] (App (Con eq_ind 0) [Var 0, Var 1]))
      weird1 = Test.assertEq (Set.fromList [0, 1]) weird_free
      
  conj3_t <- Simp.run (conjunction 3)
  conj3_t' <- Test.simplifiedTerm "fun (p q r: bool) -> and p (and q r)"
  let conj1 = Test.assertEq conj3_t' conj3_t
      
  eq_nat <- Test.simplifiedTerm "eq[nat]"
  eq_ntree <- Test.simplifiedTerm "eq[ntree]"
  eq_bool <- Test.simplifiedTerm "eq[bool]"
  eq_unit <- Test.simplifiedTerm "eq[unit]"
  
  eq_nat' <- Test.simplifiedTerm def_eq_nat
  eq_ntree' <- Test.simplifiedTerm def_eq_ntree
  eq_bool' <- Test.simplifiedTerm def_eq_bool
  eq_unit' <- Test.simplifiedTerm def_eq_unit
  
  let eq1 = Test.assertEq eq_nat' eq_nat 
      eq2 = Test.assertEq eq_ntree' eq_ntree 
      eq3 = Test.assertEq eq_bool' eq_bool 
      eq4 = Test.assertEq eq_unit' eq_unit 
      
  Lam bind_y add_simp <- Test.simplifiedTerm def_add_raw
  add_raw <- Test.term def_add_raw
  express1 <- Env.bind bind_y $ do
    App add_raw' [_] <- expressFreeVariable 0 add_simp
    Test.assertTermEq add_raw add_raw'
      
  return $ Test.list $
    [ dec1, dec2, dec3
    , fin1, fin2, fin3 
    , fold1, fold2
    , weird1
    , conj1
    , eq1, eq2, eq3, eq4
    , express1
    ]

def_eq_unit, def_eq_bool, def_eq_nat, def_eq_ntree :: String
def_eq_unit = 
  "fun (u v: unit) -> True"
def_eq_bool =
  "fun (p q: bool) -> if p then q else not q"
def_eq_nat =
  "fix (eq: nat -> nat -> bool) (x y: nat) -> "
  ++ "match x with "
  ++ "| 0 -> match y with | 0 -> True | Suc y' -> False end "
  ++ "| Suc x' -> match y with | 0 -> False | Suc y' -> eq x' y' end "
  ++ "end"
def_eq_ntree =
  "fix (eq: ntree -> ntree -> bool) (t t': ntree) -> "
  ++ "match t with "
  ++ "| Leaf -> match t' with | Leaf -> True | Node t1' x' t2' -> False end "
  ++ "| Node t1 x t2 -> match t' with "
    ++ "| Leaf -> False "
    ++ "| Node t1' x' t2' -> and (eq t1 t1') (and (eq[nat] x x') (eq t2 t2')) "
    ++ "end end"
    
def_add_raw =
  "fix (add: nat -> nat -> nat) (y: nat) (x: nat) -> "
  ++ "match x with | 0 -> y | Suc x' -> Suc (add y x') end"
    
