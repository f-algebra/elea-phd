module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Bind (..), Type (..) )
import Elea.Term ( Term (..), Alt (..) )
import qualified Elea.Term as Term
import qualified Elea.Type as Type
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs

tests = Test.label "Parser" 
    $ Test.run $ do
  Test.loadPrelude
  t_nat <- Parse.ty "nat"
  t_suc <- Parse.term "Suc"
  t_add <- Parse.term "add"
  return
    $ Test.list 
    [ Test.assertEq nat t_nat
    , Test.assertEq suc t_suc 
    , Test.assertEq add t_add ]
  where
  bind = Bind Nothing
  
  nat = Ind (bind Set) 
    [ bind (Type.Var 0)
    , bind (Fun (bind (Type.Var 0)) (Type.Var 1)) ]
  
  suc = Inj 1 nat

  add_ty = Fun (bind nat) (Fun (bind nat) nat)
  add = Fix (bind add_ty)
    $ Lam (bind nat) 
    $ Lam (bind nat)
    $ Case (Term.Var 1) nat
    [ Alt [] (Term.Var 0)
    , Alt [bind nat] (suc `Term.App` 
        (Term.unflattenApp [Term.Var 3, Term.Var 0, Term.Var 1])) ]
