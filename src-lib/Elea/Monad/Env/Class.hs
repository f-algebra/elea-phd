{-# LANGUAGE UndecidableInstances #-}
-- | The type and pattern match environment monad class.
module Elea.Monad.Env.Class
(
  Write (..), 
  bind, bindMany,
 
  Bindings (..), 
  boundAt, bindingDepth, isBound,
  
  Matches (..),
  findMatches,
  
  Full,
  
  Tracks (..), 
  trackeds, liftTracked, 
  offset, liftByOffset, 
  lowerByOffset, lowerableByOffset, tryLowerByOffset,
)
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Unification ( Unifier )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map


-- | A writable type and pattern match environment. You don't write
-- definitions here, definitions are available globally.
class Monad m => Write m where
  -- | Bind a variable index to a type within the environment
  bindAt :: Index -> Bind -> m a -> m a
  
  -- | Declare that the first term has been pattern matched to the second
  matched :: Term -> Term -> m a -> m a
  
  
-- | Bind a variable at index 0
bind :: Write m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Write m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

-- | An environment which tracks de-Bruijn index type bindings.
class Write m => Bindings m where
  -- | Get the stack of type bindings. This is morally a mapping from
  -- de-Bruijn indices to bindings, but is best represented as a list,
  -- so "map lookup" becomes "list indexing".
  bindings :: m [Bind]
  
  
-- | An environment which tracks pattern matches.
class Write m => Matches m where
  -- | The pattern matches that 
  -- have been made within the environment at this point.
  matches :: m [(Term, Term)]
  

-- | An environment that tracks both bindings and pattern matches
type Full m = (Bindings m, Matches m)
  

-- | Lookup the type of a variable. 
boundAt :: Bindings m => Index -> m Bind
boundAt at = do
  bs <- bindings
  if at >= length bs
  then error $ "Cannot retrieve the binding for index " ++ show at
    ++ " in an environment with only " ++ show (length bs :: Int) 
    ++ " bindings."
  else return (bs !! at)
  
-- | Is a given index bound within this environment.
isBound :: Bindings m => Index -> m Bool
isBound at = do
  bs <- bindings
  return (at < length bs)
  
-- | Returns the number of indices that have been bound.
bindingDepth :: Bindings m => m Int
bindingDepth = liftM length bindings
 
findMatches :: Matches m => (Term -> Bool) -> m [(Term, Term)]
findMatches p = liftM (filter (p . fst)) matches


-- | Anything that tracks indices as we move within something that binds
-- indices.
class (Monad m, Indexed r) => Tracks r m | m -> r where
  tracked :: m r
  liftTrackedMany :: Nat -> m a -> m a
  
trackeds :: Tracks r m => (r -> a) -> m a 
trackeds f = liftM f tracked

liftTracked :: Tracks r m => m a -> m a 
liftTracked = liftTrackedMany 1

offset :: (Enum e, Tracks e m) => m Nat
offset = trackeds enum

liftByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m a
liftByOffset x = liftM (flip Indices.liftMany x) offset

lowerByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m a
lowerByOffset x = liftM (flip Indices.lowerMany x) offset

lowerableByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m Bool
lowerableByOffset x = liftM (flip Indices.lowerableBy x) offset 

tryLowerByOffset :: (Fail.Can m, Enum e, Tracks e m, Indexed a) => a -> m a
tryLowerByOffset x = do
  can <- lowerableByOffset x
  Fail.unless can
  lowerByOffset x


-- * Various generic instances for these type classes

instance (Monoid w, Write m) => Write (WriterT w m) where
  bindAt at b = mapWriterT (bindAt at b)
  matched t w = mapWriterT (matched t w)
  
instance (Monoid w, Bindings m) => Bindings (WriterT w m) where
  bindings = Trans.lift bindings
             
instance Monad m => Write (ReaderT [Bind] m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ _ = id
  
instance Monad m => Bindings (ReaderT [Bind] m) where 
  bindings = ask
  
instance Write m => Write (MaybeT m) where
  bindAt at b = mapMaybeT (bindAt at b)
  matched t w = mapMaybeT (matched t w)
  
instance Bindings m => Bindings (MaybeT m) where
  bindings = Trans.lift bindings
  
instance Matches m => Matches (MaybeT m) where
  matches = Trans.lift matches
  
instance Write m => Write (EitherT e m) where
  bindAt at b = mapEitherT (bindAt at b)
  matched t w = mapEitherT (matched t w)
  
instance Bindings m => Bindings (EitherT e m) where
  bindings = Trans.lift bindings
  
instance Matches m => Matches (EitherT e m) where
  matches = Trans.lift matches

instance Write m => Write (StateT s m) where
  bindAt at b = mapStateT (bindAt at b)
  matched t w = mapStateT (matched t w)
  
instance Bindings m => Bindings (StateT s m) where
  bindings = Trans.lift bindings

instance Matches m => Matches (StateT s m) where
  matches = Trans.lift matches
  
instance Tracks r m => Tracks r (MaybeT m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapMaybeT (liftTrackedMany n)
  
instance (Monoid w, Tracks r m) => Tracks r (WriterT w m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapWriterT (liftTrackedMany n)
