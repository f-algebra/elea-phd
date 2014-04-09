module Elea.Tests.Inventor
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Type
import Elea.Show ( showM )
import qualified Elea.Terms as Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fusion.Class as Fusion
import qualified Elea.Fixpoint as Fix
import qualified Elea.Inventor as Invent
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set

testFixFix :: forall m . (Defs.Read m, Env.Read m, Fusion.Memo m)
  => Term -> m Test.Test
testFixFix term =
  Env.bindMany bs (fixFix inner_t)
  where
  (bs, inner_t) = flattenLam (Simp.run term)
  
  fixFix :: Term -> m Test.Test
  fixFix term@(App o_fix [o_arg@(App i_fix i_args)]) = do
    -- Take a composition of two fixpoints, fuse them, 
    -- then split them with invention.
    fused <- Fail.successM 
      $ Fix.fusion (return . Simp.run) ctx i_fix
    invented <- Fail.successM 
      $ Invent.run (return . Simp.run) o_arg fused
    
    let test1 = Test.assertSimpEq term (Context.apply invented o_arg)
    return (Test.list [ test1 ])
    where
    ctx = Context.make (\gap_f -> App o_fix [App gap_f i_args])
    
  fixFix other = do
    other_s <- showM other
    error other_s
    

tests = id
    . Test.label "Inventor"
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
    $ Invent.run (return . Simp.run) append_t append_t
  let id_fold' = Context.toLambda (Base nlist) id_fold_ctx
      test_id = Test.assertSimpEq id_fold id_fold'
  
  appapp <- Test.term 
    $ "fun (xs: nlist) (ys: nlist) (zs: nlist) -> "
      ++ "append (append xs ys) zs"
  test_appapp <- testFixFix appapp
  
  filterapp <- Test.term
    $ "fun (p: nat -> bool) (xs: nlist) (x: nat) -> "
      ++ "append (filter p xs) (Cons x Nil)"
  test_filterapp <- testFixFix filterapp
  
  return
    . Test.list
    $ [ test_id
      , test_appapp
      , test_filterapp
      ]
