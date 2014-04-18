module Elea.Tests.UMap
(
  tests
)
where

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
    
    let test1 = Test.assertEq (pure app1 :: Generalised Term) (pure app2)
    
    let Just (uni, msg) = id
          . UMap.lookup app2
          . UMap.insert app1 "hurrah" 
          $ UMap.empty
        test2 = Test.assertEq "hurrah" msg
    
    return (Test.list [test1, test2])
