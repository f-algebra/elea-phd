module Elea.Tests.Context
(
  tests
)
where

import Prelude ()
import Elea.Prelude             
import Elea.Index
import qualified Elea.Context as Context
import qualified Elea.Testing as Test
import qualified Elea.Term as Term
import qualified Elea.Type as Type

tests = Test.label "Contexts"                                          
    $ Test.run $ do
  Test.loadPrelude
  
  ctx1 <- liftM Context.fromLambda 
        $ Test.term t1_ctx
  sub1 <- Test.term t1_sub
  let app1 = Context.apply ctx1 sub1
  aim1 <- Test.term t1_aim
  let test1 = Test.assertEq aim1 app1
      
  let Just sub1' = Context.remove ctx1 aim1
      test1' = Test.assertEq sub1 sub1'
      
  return 
    $ Test.list [ test1, test1' ]          
  where
  t1_ctx = "fun (gap:nat) (y:nat) -> add gap y"
  t1_sub = "mul 1 2"
  t1_aim = "fun (y:nat) -> add (mul 1 2) y"
                                                                                           
  
