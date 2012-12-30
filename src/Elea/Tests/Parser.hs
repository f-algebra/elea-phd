module Elea.Tests.Parser 
(
  tests
) 
where

import Prelude ()
import Elea.Prelude hiding ( assert ) 
import Elea.Parser ( ELisp (..) )
import qualified Elea.Testing as Test
import qualified Elea.Parser as Parser
import qualified Control.Failure as Fail

tests = Test.label "Parser" 
  $ Test.list 
  $ [ test_elisp ]

test_elisp = 
  Test.list [
  
 
  ] 
  where
  test1_txt = "(
  test1_lisp = 
