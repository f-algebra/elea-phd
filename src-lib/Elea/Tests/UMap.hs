module Elea.Tests.UMap
(
  tests
)
where

import Elea.Prelude
import Elea.Term
import Elea.Term.Ext
import Elea.Type
import Elea.Show
import Elea.Unification.Map ( UMap, Generalised )
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Testing as Test
import qualified Elea.Unification.Map as UMap
import qualified Data.Set as Set

tests = Test.label "UMap"
    $ Test.run $ do
  Test.loadPrelude
  
  Test.localVars "(xs ys: list<nat>) (x n m: nat)" $ do
    app1 <- Test.simplifiedTerm "append<nat> xs ys"
    app2 <- Test.simplifiedTerm "append<nat> xs (Cons<nat> x Nil<nat>)"
    
    let test1 = Test.assertEq (pure app1 :: Generalised Term) (pure app2)
    
    let Just (uni, msg) = id
          . UMap.lookupLG app2
          . UMap.insert app1 "hurrah" 
          $ UMap.empty
        test2 = Test.assertEq "hurrah" msg
        
    leq_nat1 <- 
      Test.simplifiedTerm "assert False <- leq_nat x n in leq_nat m x"
    leq_nat2 <-
      Test.simplifiedTerm "assert False <- leq_nat x n in leq_nat n x"
      
    let leq_map = id
          . UMap.insert leq_nat1 "not true either"
          . UMap.insert leq_nat2 "actually true"
          . UMap.insert leq_nat1 "not true"
          $ UMap.empty
          
        Just (_, msg3) = UMap.lookupLG leq_nat2 leq_map
        test3 = Test.assertEq "actually true" msg3
        Just (_, msg4) = UMap.lookupLG leq_nat1 leq_map
        test4 = Test.assertEq "not true either" msg4
        
        leq_map2 = id
          . UMap.insert leq_nat1 "meep"
          $ UMap.empty
          
        mby5 = UMap.lookupAlphaEq leq_nat2 leq_map2
        test5 = Test.assertEq Nothing mby5
    
    return (Test.list [test1, test2, test3, test4, test5])
