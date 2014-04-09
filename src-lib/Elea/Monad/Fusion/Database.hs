-- | A database on ongoing, completed and failed fusion attempts.
-- Threaded as state through Elea.
module Elea.Monad.Fusion.Database
(
  Database,
  empty,
  lookup,
  insert,
  
  Outcome (..),
  maybeToOutcome
)
where

import Prelude ()
import Elea.Prelude hiding ( lookup )
import Elea.Term
import Elea.Unification ( Unifier )
import Elea.Monad.Env ()
import Elea.Show
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Unification.Map as UMap
import qualified Elea.Monad.Failure.Class as Fail

data Outcome
  = Pending
  | Failure
  | Success !Term
  deriving ( Eq, Ord, Show )

data Database
  = Database { _dbFusions :: UMap.UMap Term Outcome }
  deriving ( Show )

mkLabels [ ''Database ]

empty :: Database
empty = Database UMap.empty
  
lookup :: Fail.Can m => Term -> Database -> m Outcome
lookup term db = do
  (uni, outcome) <- UMap.lookup term (get dbFusions db)
  return (mapTerms (Unifier.apply uni) outcome)

insert :: Term -> Outcome -> Database -> Database
insert term outcome = modify dbFusions (UMap.insert term outcome)

maybeToOutcome :: Maybe Term -> Outcome
maybeToOutcome Nothing = Failure
maybeToOutcome (Just t) = Success t

instance ContainsTerms Outcome where
  mapTermsM _ Pending = return Pending
  mapTermsM _ Failure = return Failure
  mapTermsM f (Success t) =
    liftM Success (f t)

