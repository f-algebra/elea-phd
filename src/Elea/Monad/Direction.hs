module Elea.Monad.Direction 
(
  Direction (..),
  Has (..),
  invert,
  prover,
  requireInc,
  requireDec
)
where

import Elea.Prelude hiding ( get, set, local, reverse, invert )
import qualified Elea.Monad.Failure.Class as Fail

-- | Whether this rewrite can make terms less or more defined
data Direction = Inc | Dec | Eq
  deriving ( Eq )

reverse :: Direction -> Direction
reverse Dec = Inc
reverse Inc = Dec
reverse Eq = Eq


class Monad m => Has m where
  get :: m Direction
  local :: Direction -> m a -> m a
  
invert :: Has m => m a -> m a
invert m = do
  d <- get
  local (reverse d) m
  
require :: (Has m, Fail.Can m) => Direction -> m ()
require d = do
  d' <- get
  Fail.when (d /= d') 

requireDec, requireInc :: (Has m, Fail.Can m) => m ()
requireInc = require Inc
requireDec = require Dec

prover :: Has m => m a -> m a
prover = local Inc

