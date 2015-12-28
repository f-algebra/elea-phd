-- Counting and limiting computation step count (whatever this means).
-- Imported as Steps
module Elea.Monad.StepCounter
(
  Counter (..),
  Limiter (..),
  anyRemaining
)
where

import Elea.Prelude hiding ( take )
import qualified Control.Monad.Trans.Class as Trans

class Monad m => Counter m where
  take :: m ()
  taken :: m a -> m (a, Nat)

class Counter m => Limiter m where
  limit :: CoNat -> m a -> m a
  remaining :: m CoNat

anyRemaining :: Limiter m => m Bool
anyRemaining = do
  n <- remaining
  return (n > 0)

instance Counter m => Counter (ReaderT r m) where
  take = Trans.lift take
  taken = mapReaderT taken

instance Limiter m => Limiter (ReaderT r m) where
  limit n = mapReaderT (limit n)
  remaining = Trans.lift remaining

instance Counter m => Counter (MaybeT m) where
  take = Trans.lift take
  taken = mapMaybeT maybeTaken
    where 
    maybeTaken :: m (Maybe a) -> m (Maybe (a, Nat))
    maybeTaken run = do
      (mby_a, n) <- taken run
      if isNothing mby_a
      then return Nothing
      else return (Just (fromJust mby_a, n))

instance Limiter m => Limiter (MaybeT m) where
  limit n = mapMaybeT (limit n)
  remaining = Trans.lift remaining