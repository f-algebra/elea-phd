module Elea.Tests.Context
(
  tests
)
where

import Prelude ()
import Elea.Prelude             
import Elea.Index
import Elea.Monad.Elea ( Elea )
import qualified Elea.Context as Context
import qualified Elea.Testing as Test

tests = Test.label "Contexts"                                          
    $ Test.run $ do
  Test.loadPrelude
  
  test1 <- contextTest ctx1 sub1 aim1
  test2 <- contextTest ctx2 sub2 aim2
  
  return $ Test.list [test1, test2]       
  where
  ctx1 = "fun (gap:nat) (y:nat) -> add y gap"
  sub1 = "mul 1 2"
  aim1 = "fun (y:nat) -> add y (mul 1 2)"
  
  ctx2 = "fun (gap:nat) (x:nat) -> "
    ++ "match x with | 0 -> fun (y:nat) -> mul gap y "
    ++ "| Suc x' -> fun (y:nat) -> mul x' gap end"
  sub2 = "add 2 2"
  aim2 = "fun (x:nat) -> match x with "
    ++ "| 0 -> fun (y:nat) -> mul (add 2 2) y "
    ++ "| Suc x' -> fun (y:nat) -> mul x' (add 2 2) end"
                                    
  
contextTest :: String -> String -> String -> Elea Test.Test
contextTest ctx_s sub_s aim_s = do
  ctx <- liftM Context.fromLambda (Test.term ctx_s)
  sub <- Test.term sub_s
  aim <- Test.term aim_s
  let app = Context.apply ctx sub
      test1 = Test.assertEq aim app
      Just sub' = Context.remove ctx aim
      test2 = Test.assertEq sub sub'
  return $ Test.list [test1, test2]
  
