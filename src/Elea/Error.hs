module Elea.Error
(
  Monad, Type,
  throw, catch, fromEither,
  none, wasThrown
)
where

import Prelude ()
import Elea.Prelude hiding ( Monad, catch )
import qualified Elea.Prelude as Prelude

type Type = String

class Prelude.Monad m => Monad m where
  throw :: Type -> m a
  catch :: m a -> (Type -> m a) -> m a

fromEither :: (Monad m, Show a) => Either a b -> m b
fromEither (Left err) = throw (show err)
fromEither (Right val) = return val

none :: Either Type b -> b
none = fromRight

wasThrown :: Either Type b -> Bool
wasThrown = isLeft

instance Monad (Either Type) where
  throw = Left
  
  catch (Left err) handle = handle err
  catch right _ = right
  
instance Monad m => Monad (ReaderT r m) where
  throw = lift . throw
  
  catch rdr handle = 
    ReaderT $ \r -> catch (runReaderT rdr r) 
            $ \e -> runReaderT (handle e) r
  
