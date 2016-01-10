-- | Everything to do with loading .elea files into the Fedd monad. Provides the 
-- very useful Read instance for Term, as this must include prelude.elea in 
-- order to be able to read useful types/functions. 
module Elea.Monad.Fedd.Include
  ( loadPrelude, evalWithPrelude )
where

import Elea.Prelude
import Elea.Type hiding ( get )
import Elea.Term
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Parser.Calculus as Parse
import qualified Elea.Monad.Definitions.Data as Defs
import qualified Elea.Monad.Error.Class as Err
import qualified System.IO.Unsafe as Unsafe
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

preludeDefs :: Defs.Data
preludeDefs = Fedd.eval loadPrelude
  where
  prelude_src = id
    . Unsafe.unsafePerformIO
    $ readFile "prelude.elea"

  loadPrelude :: Fedd.Fedd Defs.Data
  loadPrelude = do
    Err.noneM 
      . liftM (map uninterpreted) 
      $ Parse.program prelude_src
    Fedd.getDefinitions

loadPrelude :: Monad m => Fedd.FeddT m ()
loadPrelude = Fedd.setDefinitions preludeDefs

evalWithPrelude :: Fedd.Fedd a -> a
evalWithPrelude =
  Fedd.eval . (loadPrelude >>)

readWithPrelude :: String -> Term
readWithPrelude term_def = id
  . evalWithPrelude 
  . Err.noneM 
  $ Parse.term term_def

instance Read Term where
  readPrec = do
    term_def <- ReadPrec.look
    let term = readWithPrelude term_def    
    -- Seems like a decent enough place to check that "read . show == id"
    assertEq "read . show == id" term (readWithPrelude (show term))
      $ return term
