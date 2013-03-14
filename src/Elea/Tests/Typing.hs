module Elea.Tests.Typing 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), Alt (..) )
import qualified Elea.Term as Term
import qualified Elea.Typing as Typing
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs

tests = Test.label "Typing" 
    $ Test.run $ do
  Test.loadPrelude
  
  add <- Parse.term "add"
  add_ty <- Parse.ty "nat -> nat -> nat"
  add_ty' <- Typing.typeOf add
  
  rev <- Parse.term "rev"
  rev_ty <- Parse.ty "(a:*) -> list a -> list a"
  rev_ty' <- Typing.typeOf rev
  
  return
    $ Test.list 
    [ Test.assertEq add_ty add_ty'
    , Test.assertEq rev_ty rev_ty' ]

