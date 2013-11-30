module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Type
import qualified Elea.Type as Type
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs

tests = Test.label "Parser" 
    $ Test.run $ do
  Test.loadPrelude
  t_suc <- Test.term "Suc"
  t_add <- Test.term "add"
  return
    $ Test.list 
    [ Test.assertEq suc t_suc 
    , Test.assertEq add t_add ]
  where
  nat = Ind "nat" [("0", []), ("Suc", [IndVar])] 
  nat_ty = Base nat
  suc = Con nat 1

  add_ty = Type.unflatten [nat_ty, nat_ty, nat_ty]
  add = Fix (Bind "+" add_ty)
    $ Lam (Bind "x" nat_ty)
    $ Lam (Bind "y" nat_ty)
    $ Case nat (Var 1)
    [ Alt [] (Var 0)
    , Alt [Bind "x'" nat_ty] (App suc 
        [App (Var 3) [Var 0, Var 1]]) ]
