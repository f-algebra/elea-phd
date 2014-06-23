module Elea.Monad.Fusion.Class
(
  Memo (..),
  memoise,
)
where

import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Failure.Class as Fail

-- | Memoising fusion transformations
class Monad m => Memo m where
  memoiseMaybe :: ()
    => Context 
    -> Name 
    -> Name 
    -> m (Maybe Term) -> m (Maybe Term)

memoise :: (Fail.Can m, Memo m) => Context -> Name -> Name -> m Term -> m Term
memoise ctx name name' run = do
  mby_t <- memoiseMaybe ctx name name' (Fail.catch run)
  Fail.when (isNothing mby_t)
  return (fromJust mby_t)

instance Memo m => Memo (MaybeT m) where
  memoiseMaybe ctx n n' = 
    mapMaybeT (liftM return . memoiseMaybe ctx n n' . liftM join)
  
instance Memo m => Memo (Env.TrackMatches m) where
  memoiseMaybe ctx n n' = 
    Env.mapTrackMatches (memoiseMaybe ctx n n')
    
instance (Indexed r, Memo m) => Memo (Env.AlsoTrack r m) where
  memoiseMaybe ctx n n' = 
    Env.mapAlsoTrack (memoiseMaybe ctx n n')
