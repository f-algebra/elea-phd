module Elea.Foldable.WellFormed 
  ( WellFormed (..) )
where

import Elea.Prelude hiding ( assert )
import Elea.Monad.Error.Assertion ( Assert )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Elea.Foldable as Fold

class (Fold.Refoldable t, PrintfArg t, Foldable (Fold.Base t)) => WellFormed t where
  assertLocal :: t -> Assert

  assert :: t -> Assert
  assert = Fold.para phi
    where
    phi :: forall t . WellFormed t => Fold.Base t (t, Assert) -> Assert
    phi = id
      . Assert.firstFailure 
      . toList 
      . map getTrace
      where
      getTrace :: (t, Assert) -> Assert
      getTrace (t, inner) = do
        Assert.augment (read msg) inner
        where
        msg = printf "not well-formed %n" t
