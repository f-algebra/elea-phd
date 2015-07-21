module Elea.Monad.Fusion
(
  Env (..),
  findTags,
)
where

import Elea.Prelude hiding ( local )
import Elea.Term
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Set as Set

class Monad m => Env m where
  local :: Tag -> Term -> Index -> m a -> m a
  rewrites :: m [(Tag, Term, Index)]
  
findTags :: Env m => Set Tag -> m [(Term, Index)]
findTags tags = id
  . liftM (map (\(_, f, t) -> (f, t))) 
  . liftM (filter (\(t, _, _) -> Set.member t tags)) 
  $ rewrites

instance Env m => Env (MaybeT m) where
  local a t x = mapMaybeT (local a t x)
  rewrites = Trans.lift rewrites
  
