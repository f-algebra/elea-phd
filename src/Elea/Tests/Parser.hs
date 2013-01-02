module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), InnerTerm (..), Alt (..) )

import qualified Elea.Testing as Test
import qualified Elea.Parser as Parse
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error as Error

tests = Test.label "Parser" 
    $ Test.run $ do
  Test.loadPrelude
  Just suc <- Defs.lookup "suc"
  Just plus <- Defs.lookup "+"
  return
    $ Test.list
    [ Test.assert (suc == Term mempty (Inj 1))
    , Test.assert (plus == plus_def) ]
  where
  t = Term mempty
  idx n = t (Var (toEnum n))
  
  suc_branch = t $ App (t $ Inj 1) $ t 
    $ App (idx 3) (t $ App (idx 0) (idx 1))
  plus_def = t $ Fix $ t $ Lam $ t $ Lam $ t $ Case (idx 1) 
    [ Alt 0 (idx 0)
    , Alt 1 suc_branch ]


