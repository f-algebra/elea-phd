-- | Constructing and using computations which can fail.
-- Like 'MonadPlus' without 'mplus', and with more readably named functions.
-- Requires qualified import (usually as "Fail").
module Elea.Monad.Failure (
  Monad (..), when, unless, toMaybe, 
  success, successM, catchWith, fromEither, has
) where

import Prelude ()
import Elea.Prelude hiding ( catch, Monad, when, unless )
import qualified Elea.Prelude as Prelude

-- | Use qualified import to get "Fail.Monad"
class Prelude.Monad m => Monad m where
  -- | The computation fails if this point is reached, like 'mzero'.
  here :: m a

-- | The computation fails if the argument is 'True'.
when :: Monad m => Bool -> m ()
when = flip Prelude.when here

-- | The computation fails if the argument is 'False'.
unless :: Monad m => Bool -> m ()
unless = flip Prelude.unless here

fromEither :: Monad m => Either a b -> m b
fromEither (Left _) = here
fromEither (Right x) = return x

has :: Maybe a -> Bool
has = isNothing

-- | Strips the 'Fail.Monad' by assuming that a computation has succeeded, 
-- will throw an error if this is not the case.
success :: Maybe a -> a
success Nothing = 
  error "Failure.success was given a failed computation"
success (Just x) = x

-- * The following functions remove the instance of 'MonadFailure'
-- from some computation, by handling the failure in some way.
-- They work by instantiating this 'MonadFailure' as a 'MaybeT' and
-- handling the maybe.

-- | Converts a 'MonadFailure' instance into a 'Maybe'
toMaybe :: Prelude.Monad m => MaybeT m a -> m (Maybe a)
toMaybe = runMaybeT

-- | Strips the 'Fail.Monad' by assuming that a computation has succeeded, 
-- will throw an error if this is not the case.
successM :: Prelude.Monad m => MaybeT m a -> m a
successM mby_t = do
  mby <- runMaybeT mby_t
  Prelude.when (isNothing mby)
    $ error "Failure.successT was given a failed computation"
  return (fromJust mby)

-- | Strips 'Fail.Monad' by providing a default value to return
-- if the computation has failed.
catchWith :: Prelude.Monad m => a -> MaybeT m a -> m a
catchWith def = liftM (fromMaybe def) . toMaybe
  

-- * Instance declarations

instance Prelude.Monad m => Monad (MaybeT m) where
  here = mzero

instance Monad Maybe where
  here = mzero
  
instance (Monoid w, Monad m) => Monad (WriterT w m) where
  here = lift here
  
instance Monad m => Monad (ReaderT r m) where
  here = lift here
  
