module Elea.Monad.Transform.Signals (
  Env (..), Signals (..), 
  stopRewriting, 
  usedAntecedentRewrite, 
  didRewrite
) where

import Elea.Prelude 

data Signals = Signals 
  { _stopRewriting :: Bool
  , _didRewrite :: Bool
  , _usedAntecedentRewrite :: Bool }

mkLabels [ ''Signals ]

instance Monoid Signals where
  mempty = Signals False False False
  mappend (Signals p1 p2 p3) (Signals q1 q2 q3) =
    Signals (p1 || q1) (p2 || q2) (p3 || q3)

class Monad m => Env m where
  tellStopRewriting :: m ()
  tellUsedAntecentRewrite :: m ()
  tellDidRewrite :: m ()
  consume :: m a -> m (a, Signals)
  listen :: m a -> m (a, Signals)
