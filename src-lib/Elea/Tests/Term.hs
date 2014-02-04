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
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp
import qualified Elea.Definitions as Defs
import qualified Data.Set as Set

tests = Test.label "Terms"
    $ Test.run $ do
  Test.loadPrelude
  
  add <- Test.term "add"
  let (_, App add_fix@(Fix {}) _) = flattenLam add
  
  leq <- Test.term "leq"
  
  one <- Test.term "1"
  Lam _ x_list <- Test.term "fun (x: nat) -> Cons x Nil"
  xs_list <- liftM (\t -> app t [Var 0]) (Test.term "Cons 1")
   
  let dec1 = Test.assertEq [0] (decreasingArgs add_fix)
      dec2 = Test.assertEq [0, 1] (decreasingArgs leq)
      
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
      
  take_fix <- Test.simplifiedTerm "take"
  height_fix <- Test.simplifiedTerm "height"
  flat_fix <- Test.simplifiedTerm "flatten"
  rev_fix <- Test.simplifiedTerm "reverse"
  
  let prod1 = Test.assert (isProductive take_fix)
      prod2 = Test.assert (isProductive height_fix)
      prod3 = Test.assertNot (isProductive flat_fix)
      prod4 = Test.assertNot (isProductive rev_fix)
    
  return $ Test.list $
    [ dec1, dec2
    , fin1, fin2, fin3 
    , fold1, fold2
    , prod1, prod2, prod3
    ]

