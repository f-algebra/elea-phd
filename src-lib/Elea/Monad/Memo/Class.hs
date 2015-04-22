module Elea.Monad.Memo.Class
(
  Can (..),
  memo,
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import Elea.Transform.Names ( Name )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Failure.Class as Fail

class Monad m => Can m where
  maybeMemo :: Name -> Term -> m (Maybe Term) -> m (Maybe Term)

memo :: (Fail.Can m, Can m) => Name -> Term -> m Term -> m Term
memo n t = 
  Fail.joinMaybe . maybeMemo n t . Fail.catch
  

instance Can m => Can (MaybeT m) where
  maybeMemo n t = 
    mapMaybeT (liftM Just . maybeMemo n t . liftM join)
    
    
instance Can m => Can (Env.TrackMatches m) where
  maybeMemo n t = 
    Env.mapTrackMatches (maybeMemo n t)
    
    
instance (Indexed r, Can m) => Can (Env.AlsoTrack r m) where
  maybeMemo n t = 
    Env.mapAlsoTrack (maybeMemo n t)

