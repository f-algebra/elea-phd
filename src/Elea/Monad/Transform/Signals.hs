module Elea.Monad.Transform.Signals (
  Env (..), Signals (..), 
  stopRewriting, usedAntecedentRewrite
) where

import Elea.Prelude 

data Signals = Signals 
  { _stopRewriting :: Bool
  , _usedAntecedentRewrite :: Bool }

mkLabels [ ''Signals ]

instance Empty Signals where
  empty = Signals False False

class Monad m => Env m where
  tellStopRewriting :: m ()
  tellUsedAntecentRewrite :: m ()
  consume :: m a -> m (a, Signals)
