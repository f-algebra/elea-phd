module Elea.Foldable.WellFormed 
  ( WellFormed (..), LocallyWellFormed (..) )
where

import Elea.Prelude hiding ( assert )
import Elea.Monad.Error.Assertion ( Assert )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Elea.Foldable as Fold

class (Fold.Refoldable t, PrintfArg t, Foldable (Fold.Base t)) 
    => LocallyWellFormed t where
  assertLocal :: t -> Assert

  assertAll :: t -> Assert
  assertAll = Fold.para phi
    where
    phi :: forall t . LocallyWellFormed t => Fold.Base t (t, Assert) -> Assert
    phi base = 
      Assert.augment (printf "not well-formed %n" (Fold.recover base)) $ do
        Assert.firstFailure (toList (map snd base))
        assertLocal (Fold.recover base)

class WellFormed t where
  assert :: t -> Assert
