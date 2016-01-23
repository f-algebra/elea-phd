-- | This is just a renaing/extension of 'MonadError'
module Elea.Monad.Error.Class
  ( Throws (..), ErrorT, Error, Stack
  , wasThrown, when, unless, noneM, none
  , augmentf, throwf )
where

import Elea.Prelude hiding ( catch, when, unless, print )
import qualified Elea.Prelude as Prelude
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

newtype Stack = Stack [String]
type ErrorT = EitherT Stack
type Error = ErrorT Identity

instance Show Stack where
  show (Stack stk) = id
    . intercalate ", caused by\n" 
    $ filter (not . null) stk

instance PrintfArg Stack where
  formatArg = formatArg . show

class (Monoid (Err m), Monad m) => Throws m where
  type Err m :: *
  throw :: Err m -> m a
  catch :: m a -> (Err m -> m a) -> m a
  augment :: Err m -> m a -> m a

instance Monoid Stack where
  mempty = Stack mempty
  mappend (Stack xs) (Stack ys) = Stack (xs ++ ys)

instance Read Stack where
  readsPrec _ str = [(Stack [str], "")]

instance Runnable ErrorT where
  runM et = do
    e <- runEitherT et
    case e of
      Left stk -> error (show stk)
      Right val -> return val

throwf :: (Throws m, Read (Err m), PrintfArg a) => String -> a -> m b
throwf str args = throw (readf str args)

augmentf :: (Throws m, Read (Err m), PrintfArg a) => String -> a -> m b -> m b
augmentf str args = augment (readf str args)

noneM :: Monad m => ErrorT m a -> m a
noneM = runM

none :: Error a -> a
none = run

wasThrown :: Error a -> Bool
wasThrown err = 
  case runIdentity (runEitherT err) of
    Left _ -> True
    Right _ -> False

when :: Throws m => Bool -> Err m -> m ()
when True = throw
when False = const (return ())

unless :: Throws m => Bool -> Err m -> m ()
unless = when . not

instance Monad m => Throws (ErrorT m) where
  type Err (ErrorT m) = Stack

  throw = EitherT . return . Left
  
  catch et handle = do
    e <- lift (runEitherT et)
    case e of
      Left stk -> handle stk
      right -> EitherT (return right)

  augment err = 
    EitherT . map aug . runEitherT
    where
    aug :: Either Stack a -> Either Stack a
    aug (Left stk) = Left (err ++ stk)
    aug right = right
  
instance Throws m => Throws (ReaderT r m) where
  type Err (ReaderT r m) = Err m
  throw = lift . throw
  catch = Reader.liftCatch catch
  augment e = mapReaderT (augment e)
            
instance Throws m => Throws (MaybeT m) where
  type Err (MaybeT m) = Err m
  throw = lift . throw
  catch =  Maybe.liftCatch catch
  augment e = mapMaybeT (augment e)
