module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), Alt (..) )

import qualified Elea.Term as Term
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Elea as Elea
import qualified Elea.Monad.Definitions as Defs

tests = Test.label "Parser" 
    $ Test.run $ do
  Test.loadPrelude
  return
    $ Test.list []

