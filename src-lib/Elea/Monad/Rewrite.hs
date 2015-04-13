module Elea.Monad.Rewrite
(
  Env (..),
  findTags,
)
where

import Elea.Prelude
import Elea.Term
import qualified Data.Set as Set

class Monad m => Env m where
  local :: Tag -> Term -> Index -> m a -> m a
  rewrites :: m [(Tag, Term, Index)]
  
findTags :: Env m => Set Tag -> m [(Term, Index)]
findTags tags = id
  . liftM (map (\(_, f, t) -> (f, t))) 
  . liftM (filter (\(t, _, _) -> Set.member t tags)) 
  $ rewrites

