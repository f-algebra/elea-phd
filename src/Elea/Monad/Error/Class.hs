-- | This is just a renaming of 'MonadError', and with some useful
-- new functions.
{-# LANGUAGE UndecidableInstances #-}
module Elea.Monad.Error.Class
  ( Throws (..), ErrorT, Error, Stack,
    wasThrown, when, unless )
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
    . intercalate ", caused by " 
    $ filter (not . null) stk

instance PrintfArg Stack where
  formatArg = formatArg . show

class (Monoid e, Monad m) => Throws e m | m -> e where
  throw :: e -> m a
  catch :: m a -> (e -> m a) -> m a
  augment :: e -> m a -> m a

instance Monoid Stack where
  mempty = Stack mempty
  mappend (Stack xs) (Stack ys) = Stack (xs ++ ys)

instance Read Stack where
  readPrec = do
    msg <- ReadPrec.look
    return (Stack [msg])

instance Runnable ErrorT where
  runM et = do
    e <- runEitherT et
    case e of
      Left stk -> error (show stk)
      Right val -> return val

wasThrown :: Error a -> Bool
wasThrown err = 
  case runIdentity (runEitherT err) of
    Left _ -> True
    Right _ -> False

when :: Throws e m => Bool -> e -> m ()
when True = throw
when False = const (return ())

unless :: Throws e m => Bool -> e -> m ()
unless = when . not

instance Monad m => Throws Stack (ErrorT m) where
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
  
instance Throws e m => Throws e (ReaderT r m) where
  throw = lift . throw
  catch = Reader.liftCatch catch
  augment e = mapReaderT (augment e)
            
instance Throws e m => Throws e (MaybeT m) where
  throw = lift . throw
  catch =  Maybe.liftCatch catch
  augment e = mapMaybeT (augment e)
