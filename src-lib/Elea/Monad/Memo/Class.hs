module Elea.Monad.Memo.Class
(
  Can (..),
  fusion,
  fission,
  constraintFusion,
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Failure.Class as Fail

class Monad m => Can m where
  maybeFusion :: 
    Context -> Term -> m (Maybe Term) -> m (Maybe Term)
  maybeFission ::
    Term -> Context -> m (Maybe Term) -> m (Maybe Term)
  maybeConstraintFusion ::
    Set Constraint -> Term -> m (Maybe Term) -> m (Maybe Term)

fusion :: (Fail.Can m, Can m) => Context -> Term -> m Term -> m Term
fusion ctx term = 
  Fail.joinMaybe . maybeFusion ctx term . Fail.catch
  
fission :: (Fail.Can m, Can m) => Term -> Context -> m Term -> m Term
fission term ctx = 
  Fail.joinMaybe . maybeFission term ctx. Fail.catch

constraintFusion :: (Fail.Can m, Can m) 
  => Set Constraint -> Term -> m Term -> m Term
constraintFusion cons term = 
  Fail.joinMaybe . maybeConstraintFusion cons term . Fail.catch
  

instance Can m => Can (MaybeT m) where
  maybeFusion ctx term = 
    mapMaybeT (liftM Just . maybeFusion ctx term . liftM join)
    
  maybeFission term ctx = 
    mapMaybeT (liftM Just . maybeFission term ctx . liftM join)
    
  maybeConstraintFusion term ctx = 
    mapMaybeT (liftM Just . maybeConstraintFusion term ctx . liftM join)
    
    
instance Can m => Can (Env.TrackMatches m) where
  maybeFusion ctx term = 
    Env.mapTrackMatches (maybeFusion ctx term)
    
  maybeFission ctx term = 
    Env.mapTrackMatches (maybeFission ctx term)
    
  maybeConstraintFusion ctx term = 
    Env.mapTrackMatches (maybeConstraintFusion ctx term)
    
    
instance (Indexed r, Can m) => Can (Env.AlsoTrack r m) where
  maybeFusion ctx term = 
    Env.mapAlsoTrack (maybeFusion ctx term)
    
  maybeFission ctx term = 
    Env.mapAlsoTrack (maybeFission ctx term)
    
  maybeConstraintFusion ctx term = 
    Env.mapAlsoTrack (maybeConstraintFusion ctx term)

