module Elea.Tests.Fixpoint
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Type
import qualified Elea.Terms as Term
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unifier as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp
import qualified Elea.Definitions as Defs
import qualified Elea.Fixpoint as Fix
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set

tests = id
    . Test.label "Fixpoint"
    . Test.run 
    $ do
  Test.loadPrelude
  
  cons <- Test.term "Cons"
  nil <- Test.term "Nil"
  Base nlist <- Test._type "nlist"
  
  let fold_nlist = Term.buildFold nlist (Base nlist)
      id_fold = app fold_nlist [nil, cons]
      
  append <- Test.term "append"
  
  let ([xs_b, ys_b], append_t) = flattenLam append
  id_fold_ctx <- id
    . Env.bindMany [xs_b, ys_b]
    . Fail.successM 
    $ Fix.invention Simp.run append_t append_t
  let id_fold' = Context.toLambda (Base nlist) id_fold_ctx
  
  test_id <- Test.assertSimpEq id_fold id_fold'
  
  return
    . Test.list
    $ [ test_id ]
