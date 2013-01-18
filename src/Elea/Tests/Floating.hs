module Elea.Tests.Floating
(
  tests
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term, Notes )

import qualified Elea.Testing as Test
import qualified Elea.Floating as Float
import qualified Elea.Notes.Show as Show

assertFloatEq :: Show.HasNote a => Term a -> Term a -> Test.Test
assertFloatEq = Test.assertEq `on` Float.run

tests = Test.label "Simplifier"
    $ Test.run $ do
  Test.loadPrelude
  
  t1 <- Test.term t1_str
  aim1 <- Test.term aim1_str
  let test1 = assertFloatEq aim1 t1
  
  return 
    $ Test.list
    [ test1 ]
    
  where
  t1_str = "(fix f x -> case x (0 -> lam y -> y)"
    ++ "(suc x' -> lam y -> f x' (case y (0 -> 0) (suc y' -> y'))))"
  aim1_str = "(fix f x y -> case x (0 -> y)"
    ++ "(suc x -> case y (0 -> f x 0) (suc y -> f x y)))" 
    
