module Elea.Include
  ( loadPrelude )
where

import Elea.Prelude
import Elea.Type hiding ( get )
import Elea.Term
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Parser.Calculus as Parse
import qualified Elea.Monad.Definitions.Data as Defs
import qualified Elea.Monad.Error.Class as Err
import qualified System.IO.Unsafe as Unsafe

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

instance Read Term where
  readPrec = undefined