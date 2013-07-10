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
import qualified Elea.Monad.Error as Err

tests = Test.label "Typing" 
    $ Test.run $ do
  Test.loadPrelude
 
  add <- Test.term "add"
  add_ty <- Test.term "pi nat nat -> nat"
  let add_ty' = Typing.typeOf add
        |> Err.noneM
        |> Elea.run
    
  rev <- Test.term "rev"
  rev_ty <- Test.term "pi (a:*) (list a) -> list a"
  let rev_ty' = Typing.typeOf rev
        |> Err.noneM
        |> Elea.run
    
  return
    $ Test.list
    [ Test.assertEq add_ty add_ty'
    , Test.assertEq rev_ty rev_ty' ]

