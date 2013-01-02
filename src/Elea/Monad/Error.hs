module Elea.Monad.Error
(
  Monad, Type,
  throw, catch, fromEither,
  none, noneM, wasThrown,
  when, unless,
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, catch, when, unless )
import qualified Elea.Prelude as Prelude

type Type = String

class Prelude.Monad m => Monad m where
  throw :: Type -> m a
  catch :: m a -> (Type -> m a) -> m a

fromEither :: (Monad m, Show a) => Either a b -> m b
fromEither (Left err) = throw (show err)
fromEither (Right val) = return val

none :: Either Type b -> b
none (Right x) = x
none (Left err) = error err

noneM :: Prelude.Monad m => EitherT Type m b -> m b
noneM = liftM none . runEitherT

wasThrown :: Either Type b -> Bool
wasThrown = isLeft

when :: Monad m => Bool -> Type -> m ()
when True = throw
when False = const (return ())

unless :: Monad m => Bool -> Type -> m ()
unless = when . not

instance Monad (Either Type) where
  throw = Left
  
  catch (Left err) handle = handle err
  catch right _ = right
  
instance Prelude.Monad m => Monad (EitherT Type m) where
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
  
