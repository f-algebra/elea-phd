-- | This is just a renaming of 'MonadError', and with some useful
-- new functions.
module Elea.Monad.Error.Class
(
  Can (..), Err, ErrorT,
  fromEither, augment, augmentM,
  none, noneM, wasThrown,
  when, unless, check,
)
where

import Prelude ()
import Elea.Prelude hiding ( catch, when, unless )
import qualified Elea.Prelude as Prelude
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader

type Err = String
type ErrorT = EitherT Err

class Monad m => Can m where
  throw :: Err -> m a
  catch :: m a -> (Err -> m a) -> m a
  
augmentM :: forall m a . Can m => m Err -> m a -> m a
augmentM err_m = flip catch rethrow
  where
  rethrow :: Err -> m a
  rethrow old_err = do
    new_err <- err_m
    throw (new_err ++ "\n" ++ old_err)
    
augment :: Can m => Err -> m a -> m a
augment = augmentM . return
    
check :: Monad m => (a -> m ()) -> m a -> m a
check chk ma = do
  a <- ma
  chk a
  return a

fromEither :: (Can m, Show a) => Either a b -> m b
fromEither (Left err) = throw (show err)
fromEither (Right val) = return val

none :: Either Err b -> b
none (Right x) = x
none (Left err) = error err

noneM :: Monad m => EitherT Err m b -> m b
noneM et = do
  e <- runEitherT et
  case e of
    Left err -> error err
    Right val -> return val

wasThrown :: Either Err b -> Bool
wasThrown = isLeft

when :: Can m => Bool -> Err -> m ()
when True = throw
when False = const (return ())

unless :: Can m => Bool -> Err -> m ()
unless = when . not

mapLeftT :: Can m => (a -> b) -> EitherT a m c -> EitherT b m c
mapLeftT f eth_t = EitherT $ do
  eth <- runEitherT eth_t
  return $ 
    case eth of
    Left l -> Left (f l)
    Right r -> Right r

instance Can (Either Err) where
  throw = Left
  
  catch (Left err) handle = handle err
  catch right _ = right
  
instance Monad m => Can (EitherT Err m) where
  throw = EitherT . return . Left
  
  catch et handle = do
    e <- lift (runEitherT et)
    case e of
      Left err -> handle err
      right -> EitherT (return right)
  
instance Can m => Can (ReaderT r m) where
  throw = lift . throw
  catch = Reader.liftCatch catch
            
instance Can m => Can (MaybeT m) where
  throw = lift . throw
  catch =  Maybe.liftCatch catch

  
