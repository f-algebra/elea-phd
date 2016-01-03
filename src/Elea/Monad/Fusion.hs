module Elea.Monad.Fusion
(
  Env (..),
  findTags,
  checkEnabled,
)
where

import Elea.Prelude hiding ( local )
import Elea.Term
import qualified Control.Monad.Trans.Class as Trans
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set

class Monad m => Env m where
  local :: Tag -> Term -> Term -> m a -> m a
  rewrites :: m [(Tag, Term, Term)]
  forgetRewrites :: m a -> m a
  disable :: m a -> m a
  isDisabled :: m Bool
  
findTags :: Env m => Set Tag -> m [(Term, Term)]
findTags tags = id
  . liftM (map (\(_, f, t) -> (f, t))) 
  . liftM (filter (\(t, _, _) -> Set.member t tags)) 
  $ rewrites

instance Env m => Env (MaybeT m) where
  local a t x = mapMaybeT (local a t x)
  rewrites = Trans.lift rewrites
  forgetRewrites = mapMaybeT forgetRewrites
  disable = mapMaybeT disable
  isDisabled = Trans.lift isDisabled

checkEnabled :: (Env m, Fail.Can m) => m ()
checkEnabled = do
  disabled <- isDisabled
  Fail.when disabled
  return ()
  