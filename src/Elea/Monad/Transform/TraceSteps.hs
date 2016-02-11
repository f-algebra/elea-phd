module Elea.Monad.Transform.TraceSteps 
( Env(..), traceM )
where

import Elea.Prelude hiding ( traceM )
import qualified Elea.Prelude as Prelude

class Monad m => Env m where
  enabled :: m Bool
  enable :: m a -> m a

{-# INLINE traceM #-}
traceM :: Env m => String -> m ()
#ifndef TRACE
traceM _ = return ()
#else
traceM msg = do
  should <- enabled
  when should (Prelude.traceM msg)
#endif
