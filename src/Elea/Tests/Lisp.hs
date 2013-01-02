module Elea.Tests.Lisp 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude
import Elea.Lisp ( Lisp (..) )

import qualified Elea.Testing as Test
import qualified Elea.Lisp as Lisp
import qualified Elea.Monad.Error as Error

assertParses :: String -> String -> Test.Test
assertParses txt aim = Test.label (aim ++ " == " ++ out) 
  $ Test.assert (aim == out)
  where
  out = show (Error.none (Lisp.parse txt))
  
assertFails :: String -> Test.Test
assertFails txt = Test.label ("Fail: " ++ txt)
  $ Test.assert 
  $ Error.wasThrown
  $ Lisp.parse txt
  
tests = Test.label "Lisp" 
  $ Test.list 
  [ assertParses test1 test1_aim
  , assertParses test2 test2_aim
  , assertParses test3 test3_aim
  , assertFails test4
  , assertParses test5 test5_aim
  , assertParses test6 test6_aim ] 
  where
  test1 = "(add (mul 1 2) 4)"
  test1_aim = test1
  
  test2 = "  (  !? ( H_ 34  * )  hi   q  ) "
  test2_aim = "(!? (H_ 34 *) hi q)"
  
  test3 = " hi  "
  test3_aim = "hi"
  
  test4 = "(test (fail )"
  
  test5 = " ( :a (x  :q   p y) f :b p x :c (l l) ) "
  test5_aim = "(f x :a (x y :q p) :b p :c (l l))"
  
  test6 = "(fix + x y :ty ( N -> N -> N) -> "
    ++ "case x (0 -> y) (suc x' -> suc (+ x' y)))"
  test6_aim = "(fix + x y "
    ++ "(case x (0 y) (suc x' (suc (+ x' y)))) :ty (N (N N)))"
  
  
