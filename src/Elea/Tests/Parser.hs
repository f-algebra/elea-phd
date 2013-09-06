module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs

tests = Test.label "Parser" 
    $ Test.run $ do
  Test.loadPrelude
  t_nat <- Test.term "nat"
  t_suc <- Test.term "Suc"
  t_add <- Test.term "add"
  return
    $ Test.list 
    [ Test.assertEq nat t_nat
    , Test.assertEq suc t_suc 
    , Test.assertEq add t_add ]
  where
  bind = Bind Nothing
  
  nat = Ind (bind Set) 
    [ bind (Var 0)
    , bind (Pi (bind (Var 0)) (Var 1)) ]
  
  suc = Inj 1 nat

  add_ty = Pi (bind nat) (Pi (bind nat) nat)
  add = Fix emptyFixInfo (bind add_ty)
    $ Lam (bind nat) 
    $ Lam (bind nat)
    $ Case (Var 1) nat
    [ Alt [] (Var 0)
    , Alt [bind nat] (App suc 
        [App (Var 3) [Var 0, Var 1]]) ]
