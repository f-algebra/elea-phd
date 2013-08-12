module Elea.Monad.Error
(
  Monad (..), Err, ErrorT,
  fromEither, augment, augmentM,
  none, noneM, wasThrown,
  when, unless, check,
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, catch, when, unless )
import qualified Elea.Prelude as Prelude

type Err = String
type ErrorT = EitherT Err

class Prelude.Monad m => Monad m where
  throw :: Err -> m a
  catch :: m a -> (Err -> m a) -> m a
  
augmentM :: forall m a . Monad m => m Err -> m a -> m a
augmentM err_m = flip catch rethrow
  where
  rethrow :: Err -> m a
  rethrow old_err = do
    new_err <- err_m
    throw (new_err ++ "\n" ++ old_err)
    
augment :: Monad m => Err -> m a -> m a
augment = augmentM . return
    
check :: Prelude.Monad m => (a -> m ()) -> m a -> m a
check chk ma = do
  a <- ma
  chk a
  return a

fromEither :: (Monad m, Show a) => Either a b -> m b
fromEither (Left err) = throw (show err)
fromEither (Right val) = return val

none :: Either Err b -> b
none (Right x) = x
none (Left err) = error err

noneM :: Prelude.Monad m => EitherT Err m b -> m b
noneM et = do
  e <- runEitherT et
  case e of
    Left err -> error err
    Right val -> return val

wasThrown :: Either Err b -> Bool
wasThrown = isLeft

when :: Monad m => Bool -> Err -> m ()
when True = throw
when False = const (return ())

unless :: Monad m => Bool -> Err -> m ()
unless = when . not

mapLeftT :: Monad m => (a -> b) -> EitherT a m c -> EitherT b m c
mapLeftT f eth_t = EitherT $ do
  eth <- runEitherT eth_t
  return $ 
    case eth of
    Left l -> Left (f l)
    Right r -> Right r

instance Monad (Either Err) where
  throw = Left
  
  catch (Left err) handle = handle err
  catch right _ = right
  
instance Prelude.Monad m => Monad (EitherT Err m) where
  throw = EitherT . return . Left
  
  catch et handle = do
    e <- lift (runEitherT et)
    case e of
      Left err -> handle err
      right -> EitherT (return right)
  
instance Monad m => Monad (ReaderT r m) where
  throw = lift . throw
  
  catch rdr handle = 
    ReaderT $ \r -> catch (runReaderT rdr r) 
            $ \e -> runReaderT (handle e) r

  
