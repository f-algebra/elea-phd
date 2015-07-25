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
        
    le1 <- 
      Test.simplifiedTerm "assert False <- le x n in le m x"
    le2 <-
      Test.simplifiedTerm "assert False <- le x n in le n x"
      
    let leq_map = id
          . UMap.insert le1 "not true either"
          . UMap.insert le2 "actually true"
          . UMap.insert le1 "not true"
          $ UMap.empty
          
        Just (_, msg3) = UMap.lookupLG le2 leq_map
        test3 = Test.assertEq "actually true" msg3
        Just (_, msg4) = UMap.lookupLG le1 leq_map
        test4 = Test.assertEq "not true either" msg4
        
        leq_map2 = id
          . UMap.insert le1 "meep"
          $ UMap.empty
          
        mby5 = UMap.lookupAlphaEq le2 leq_map2
        test5 = Test.assertEq Nothing mby5
        
    drop1 <- Test.term "drop<nat> n xs"
    drop2 <- Test.term "drop<nat> m ys"
    let test6 = Test.assert (Unifier.alphaEq drop1 drop2)
        drop_map = id
          . UMap.insert drop1 "drop1"
          . UMap.insert drop2 "drop2"
          $ UMap.empty
          
        [(_, (_, msg7))] = UMap.lookup drop2 drop_map
        test7 = Test.assertEq "drop1" msg7
    
    return (Test.list [test1, test2, test3, test4, test5, test6, test7])
