-- | Constructing and using computations which can fail.
-- Requires qualified import (usually as "Fail").
module Elea.Monad.Failure (
  Can (..), when, unless, toMaybe, withDefault, assert, choose,
  success, successM, catchWith, fromEither, has, fromMaybe, mapLookup,
  concatTransforms
) where

import Prelude ()
import Elea.Prelude hiding ( catch, when, unless, fromMaybe, assert )
import Control.Monad.State hiding ( unless, when )
import qualified Elea.Prelude as Prelude
import qualified Data.Monoid as Monoid
import qualified Data.Map as Map

class Monad m => Can m where
  -- | The computation fails if this point is reached, like 'mzero'.
  here :: m a
  
  -- | If the given computation has failed, return 'Nothing' (and succeed),
  -- otherwise return 'Just' the value of the original computation.
  catch :: m a -> m (Maybe a)
  

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

-- | Pick a non failing instance. If none exists then fail.
choose :: (Can m, Foldable f) => f (m a) -> m a
choose = join . liftM fromMaybe . firstM . map catch . toList

-- | Useful function since I cast the 'Maybe' of the Data.Map.lookup
-- all the time.
mapLookup :: (Ord k, Can m) => k -> Map k v -> m v
mapLookup k = fromMaybe . Map.lookup k

-- * Instance declarations

instance Monad m => Can (MaybeT m) where
  here = mzero
  catch = lift . runMaybeT

instance Can Maybe where
  here = mzero
  catch = Just
  
instance (Monoid w, Can m) => Can (WriterT w m) where
  here = lift here
  catch = mapWriterT catchW
    where
    catchW :: m (a, w) -> m (Maybe a, w)
    catchW m = do
      mby_aw <- catch m
      case mby_aw of
        Nothing -> return (Nothing, mempty)
        Just (a, w) -> return (Just a, w)
  
instance Can m => Can (ReaderT r m) where
  here = lift here
  catch = mapReaderT catch
  
instance Can m => Can (StateT s m) where
  here = lift here
  catch (runStateT -> f) = StateT $ \s -> do
    mby_as <- catch (f s)
    case mby_as of
      Nothing -> return (Nothing, s)
      Just (a, s') -> return (Just a, s')
  
instance Can m => Can (EitherT e m) where
  here = lift here
  catch = mapEitherT catchE
    where
    catchE :: m (Either e a) -> m (Either e (Maybe a))
    catchE m = do
      mby_either_a <- catch m
      case mby_either_a of
        Nothing -> return (Right Nothing)
        Just (Left e) -> return (Left e)
        Just (Right x) -> return (Right (Just x))
  
instance Monad Monoid.First where
  return = Monoid.First . return
  Monoid.First x >>= f = Monoid.First (x >>= (getFirst . f))
  
instance Can Monoid.First where
  here = Monoid.First mzero
  catch = Monoid.First . catch . Monoid.getFirst
  

-- | See the definition of @mappend@ to 
-- understand how the /transformation/ monoid works.
newtype Transform m a
  = Transform { runTransform :: a -> m a }

concatTransforms :: Can m => [a -> m a] -> a -> m a
concatTransforms = runTransform . concat . map Transform
  
instance Can m => Monoid (Transform m a) where
  -- An alternative identity element would be:
  -- > mempty = Transform return
  mempty = Transform (\_ -> here)
  
  Transform f `mappend` Transform g = Transform fg
    where
    fg x = do
      mby_fx <- catch (f x)
      case mby_fx of
        Nothing -> g x
        Just fx -> do
          mby_gfx <- catch (g fx)
          -- If f succeeded then f ++ g succeeded
          case mby_gfx of
            Nothing -> return fx
            Just gfx -> return gfx

