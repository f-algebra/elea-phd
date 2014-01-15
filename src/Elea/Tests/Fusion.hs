module Elea.Tests.Fusion
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import qualified Elea.Env as Env
import qualified Elea.Simplifier as Simp
import qualified Elea.Testing as Test
import qualified Elea.Fusion as Fusion
import qualified Elea.Monad.Definitions as Defs

checkEquation :: Equation -> Test.Test
checkEquation (Equals name bs t1 t2) = id
  . Test.label name
  . Defs.readEmpty
  . Env.emptyT
  . Env.bindMany bs $ do
    t1' <- Fusion.run t1
    t2' <- Fusion.run t2
    return (Test.assertEq t2' t1')
    
{- IDEA, could display constraints with the syntax:

match x with
| 0 -> _|_
| Suc (x': nat) -> E
end

==>

let Suc x' = x in
E

-}

  
tests = Test.label "Fusion"
    $ Test.run $ do
  Test.loadPrelude
  eqs <- Test.loadFile "src/Elea/Tests/fusion.elea"
  return
    . map checkEquation
 --   . filter ((== "count reverse") . get equationName)
    $ eqs

