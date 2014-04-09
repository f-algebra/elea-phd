module Elea.Monad.Fusion.Class
(
  Memo (..),
  memoise,
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Type
import Elea.Context ( Context )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Failure.Class as Fail

class Monad m => Memo m where
  memoiseMaybe :: Context -> Term -> m (Maybe Term) -> m (Maybe Term)

memoise :: (Fail.Can m, Memo m) => Context -> Term -> m Term -> m Term
memoise ctx term run = do
  mby_t <- memoiseMaybe ctx term (Fail.catch run)
  Fail.when (isNothing mby_t)
  return (fromJust mby_t)

instance Memo m => Memo (MaybeT m) where
  memoiseMaybe ctx term = 
    mapMaybeT (liftM out . memoiseMaybe ctx term . liftM into)
    where 
    -- The partiality of 'into' ensures the correctness of this 
    -- monad transformer.
    into :: Maybe (Maybe a) -> Maybe a
    into (Just (Just x)) = Just x
    into (Just Nothing) = Nothing
    into _ = error "Ambiguous memoiseMaybe case for MaybeT"
    
    out :: Maybe a -> Maybe (Maybe a)
    out (Just x) = Just (Just x)
    out Nothing = Just Nothing
    
  
instance Memo m => Memo (Env.TrackMatches m) where
  memoiseMaybe ctx term = 
    Env.mapTrackMatches (memoiseMaybe ctx term)
    
instance (Indexed r, Memo m) => Memo (Env.AlsoTrack r m) where
  memoiseMaybe ctx term = 
    Env.mapAlsoTrack (memoiseMaybe ctx term)
