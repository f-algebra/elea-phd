-- | This is just a renaming of 'MonadError', and with some useful
-- new functions.
module Elea.Monad.Error.Class
(
  Throws (..), Err (..), ErrorT,
  fromEither, augment, augmentM,
  none, noneM, wasThrown,
  when, unless, check,
)
where

import Elea.Prelude hiding ( catch, when, unless, print )
import qualified Elea.Prelude as Prelude
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader

type ErrorT = EitherT String
type Error = Either String

-- | Why not just use 'Show'? Because showing strings wraps them in 
-- quotes, which will just build up with multiple augment calls.
class Err e where
  print :: e -> String

class Monad m => Throws m where
  throw :: Err e => e -> m a
  catch :: m a -> (String -> m a) -> m a
  
augmentM :: forall m a e . (Throws m, Err e) => m e -> m a -> m a
augmentM err_m = flip catch rethrow
  where
  rethrow :: String -> m a
  rethrow old_err = do
    new_err <- err_m
    throw (print new_err ++ "\n" ++ old_err)
    
augment :: (Err e, Throws m) => e -> m a -> m a
augment = augmentM . return
    
check :: Monad m => (a -> m ()) -> m a -> m a
check chk ma = do
  a <- ma
  chk a
  return a

fromEither :: (Throws m, Show a) => Either a b -> m b
fromEither (Left err) = throw (show err)
fromEither (Right val) = return val

none :: Error b -> b
none (Right x) = x
none (Left err) = error err

noneM :: Monad m => ErrorT m b -> m b
noneM et = do
  e <- runEitherT et
  case e of
    Left err -> error err
    Right val -> return val

wasThrown :: Error b -> Bool
wasThrown = isLeft

when :: (Err e, Throws m) => Bool -> e -> m ()
when True = throw
when False = const (return ())

unless :: (Err e, Throws m) => Bool -> e -> m ()
unless = when . not

mapLeftT :: Throws m => (a -> b) -> EitherT a m c -> EitherT b m c
mapLeftT f eth_t = EitherT $ do
  eth <- runEitherT eth_t
  return $ 
    case eth of
    Left l -> Left (f l)
    Right r -> Right r

instance Throws (Either String) where
  throw = Left . print
  
  catch (Left err) handle = handle err
  catch right _ = right
  
instance Monad m => Throws (EitherT String m) where
  throw = EitherT . return . Left . print
  
  catch et handle = do
    e <- lift (runEitherT et)
    case e of
      Left err -> handle err
      right -> EitherT (return right)
  
instance Throws m => Throws (ReaderT r m) where
  throw = lift . throw
  catch = Reader.liftCatch catch
            
instance Throws m => Throws (MaybeT m) where
  throw = lift . throw
  catch =  Maybe.liftCatch catch
  
instance Err String where
  print = id

  
