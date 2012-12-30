module Elea.Tests.ELisp 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude hiding ( assert ) 
import Elea.ELisp ( ELisp (..) )

import qualified Elea.Testing as Test
import qualified Elea.ELisp as ELisp
import qualified Elea.Error as Error

tests = Test.label "ELisp" 
  $ Test.list 
  [ test_elisp ]

assertParses :: String -> String -> Test.Test
assertParses txt aim = Test.label txt 
  $ Test.assert 
  $ aim == show (Error.none (ELisp.parse txt))
  
assertFails :: String -> Test.Test
assertFails txt = Test.label ("Fail: " ++ txt)
  $ Test.assert 
  $ Error.wasThrown
  $ ELisp.parse txt
  
test_elisp = 
  Test.list 
  [ assertParses test1 test1_aim
  , assertParses test2 test2_aim
  , assertParses test3 test3_aim
  , assertFails test4
  , assertParses test5 test5_aim ] 
  where
  test1 = "(add (mul 1 2) 4)"
  test1_aim = test1
  
  test2 = "  (  !? ( H_ 34  * () )  hi   q  ) "
  test2_aim = "(!? (H_ 34 * ()) hi q)"
  
  test3 = " hi  "
  test3_aim = "hi"
  
  test4 = "(test (fail )"
  
  test5 = "(fix + -> \\ x y -> case x (-> y) (x' -> suc (+ x' y)))"
  test5_aim = "(fix + (\\ x y (case x ((y)) (x' (suc (+ x' y))))))"
  
  
