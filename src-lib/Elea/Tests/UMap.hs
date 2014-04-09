module Elea.Tests.UMap
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Terms
import Elea.Type
import Elea.Show
import Elea.Unification.Map ( UMap, Generalised )
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Unification.Map as UMap
import qualified Data.Set as Set

tests = Test.label "UMap"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.localVars "(xs ys: list<nat>) (x: nat)" $ do
    app1 <- Test.simplifiedTerm "append<nat> xs ys"
    app2 <- Test.simplifiedTerm "append<nat> xs (Cons<nat> x Nil<nat>)"
    
    let gen1 = UMap.generalise app1
        test1 = Test.assertEq gen1 (pure app2)
    
    let Just (uni, msg) = id
          . UMap.lookup app2
          . UMap.insert app1 "herro" 
          $ UMap.empty
        test2 = Test.assertEq "herro" msg
    
    return (Test.list [test1, test2])
