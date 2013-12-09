module Elea.Fixpoint
(
  fusion, fission
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold

Can we use the quickcheck method to see which terms allow extraction?
and hence when to apply the extraction function??


fusion :: Env.Readable m =>
  (Term -> m Term) ->
  (Index -> Context -> Term -> m Term) ->
  Context -> Term -> m Term
  
fission :: Env.Readable m =>
  (Term -> m Term)
  (
