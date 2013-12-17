-- | Constructing and using computations which can fail.
-- Like 'MonadPlus' without 'mplus', and with more readably named functions.
-- Requires qualified import (usually as "Fail").
module Elea.Monad.Failure (
  Can (..), when, unless, toMaybe, withDefault, assert,
  success, successM, catchWith, fromEither, has, fromMaybe
) where

import Prelude ()
import Elea.Prelude hiding ( catch, when, unless, fromMaybe, assert )
import qualified Elea.Prelude as Prelude
import qualified Data.Monoid as Monoid

class Monad m => Can m where
  -- | The computation fails if this point is reached, like 'mzero'.
  here :: m a
  
  -- | Pick a non failing instance. If none exists then fail.
  -- Can you implement this from just 'here'? I couldn't find a way.
  choose :: Foldable f => f (m a) -> m a
  

-- | The computation fails if the argument is 'True'.
when :: Can m => Bool -> m ()
when = flip Prelude.when here

-- | The computation fails if the argument is 'False'.
unless :: Can m => Bool -> m ()
unless = when . not

fromEither :: Can m => Either a b -> m b
fromEither (Left _) = here
fromEither (Right x) = return x

-- Force the assertion to be evaluated within the failure monad.
assert :: Can m => Bool -> m ()
assert p = Prelude.assert p (unless p)

has :: Maybe a -> Bool
has = isNothing

-- | Strips the 'Fail.Can' by assuming that a computation has succeeded, 
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
toMaybe :: Monad m => MaybeT m a -> m (Maybe a)
toMaybe = runMaybeT

withDefault :: Monad m => a -> MaybeT m a -> m a
withDefault x = liftM (Prelude.fromMaybe x) . toMaybe

-- | Strips the 'Fail.Monad' by assuming that a computation has succeeded, 
-- will throw an error if this is not the case.
successM :: Monad m => MaybeT m a -> m a
successM mby_t = do
  mby <- runMaybeT mby_t
  Prelude.when (isNothing mby)
    $ error "Failure.successT was given a failed computation"
  return (fromJust mby)

-- | Strips 'Fail.Monad' by providing a default value to return
-- if the computation has failed.
catchWith :: Monad m => a -> MaybeT m a -> m a
catchWith def = liftM (Prelude.fromMaybe def) . toMaybe

fromMaybe :: Can m => Maybe a -> m a
fromMaybe Nothing = here
fromMaybe (Just x) = return x


-- * Instance declarations

instance Monad m => Can (MaybeT m) where
  here = mzero
  choose = MaybeT . firstM . map runMaybeT . toList

instance Can Maybe where
  here = mzero
  choose = join . find isJust
  
instance (Monoid w, Can m) => Can (WriterT w m) where
  here = lift here
  choose = WriterT . choose . map runWriterT . toList
  
instance Can m => Can (ReaderT r m) where
  here = lift here
  choose (toList -> xs) = 
    ReaderT $ \r -> choose (map (flip runReaderT r) xs)
  
instance Monad Monoid.First where
  return = Monoid.First . return
  Monoid.First x >>= f = Monoid.First (x >>= (getFirst . f))
  
instance Can Monoid.First where
  here = Monoid.First mzero
  choose = Monoid.First . choose . map Monoid.getFirst . toList

