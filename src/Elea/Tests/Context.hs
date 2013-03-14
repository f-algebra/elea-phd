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
  
  Term.Lam (Type.Bind _ nat) t1_ctx <- Test.term test1_ctx
  t1_sub <- Test.term test1_sub
  t1_aim <- Test.term test1_aim
  let ctx1 = Context.make nat (flip subst t1_ctx)
      sub1 = Context.apply ctx1 t1_sub
      test1 = Test.assertEq t1_aim sub1
      
  let Just t1_sub' = Context.remove ctx1 t1_aim
      test2 = Test.assertEq t1_sub t1_sub'
      
  return 
    $ Test.list [ test1, test2 ]
  where
  test1_ctx = "fun (x:nat) (y:nat) -> add x y"
  test1_sub = "mul 1 2"
  test1_aim = "fun (y:nat) -> add (mul 1 2) y"
