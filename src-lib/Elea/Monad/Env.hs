{-# LANGUAGE UndecidableInstances #-}
-- | Type and pattern match environment monad constraints
module Elea.Monad.Env
(
  Write (..), Read (..), 
  bind, bindMany,
  boundAt, bindingDepth,
  
  MatchRead (..), 
  findMatches,
  
  Tracks (..), 
  trackeds, liftTracked, 
  offset, liftByOffset, 
  lowerByOffset, lowerableByOffset,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Unifier ( Unifier )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map


-- | A writable type and pattern match environment.
class Monad m => Write m where
  -- | Bind a variable index to a type within the environment
  bindAt :: Index -> Bind -> m a -> m a
  
  -- | Declare that the first term has been pattern matched to the second
  matched :: Term -> Term -> m a -> m a
  
bind :: Write m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Write m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind


-- | Whether you can read variable bindings from the environment. 
-- Since variables are indices, it's really a lookup function 'Nat -> Bind',
-- but this is just a list.
class Write m => Read m where
  bindings :: m [Bind]
  
-- | Lookup the type of a variable. 
boundAt :: Read m => Index -> m Bind
boundAt at = do
  bs <- bindings
  if at >= length bs
  then error $ "Cannot retrieve the binding for index " ++ show at
    ++ " in an environment with only " ++ show (length bs :: Int) 
    ++ " bindings."
  else return (bs !! enum at)
  
-- | Returns the number of indices that have been bound.
bindingDepth :: Read m => m Int
bindingDepth = liftM length bindings
 

-- | Whether you can read locally bound pattern matches from
-- an environment monad
class Write m => MatchRead m where
  matches :: m [(Term, Term)]
  
findMatches :: MatchRead m => (Term -> Bool) -> m [(Term, Term)]
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


-- * Various generic instances for these type classes

instance Write Identity where
  bindAt _ _ = id
  matched _ _ = id

instance Monad m => Write (IdentityT m) where
  -- The 'IdentityT' monad just ignores all the written type information
  bindAt _ _ = id
  matched _ _ = id

instance (Monoid w, Write m) => Write (WriterT w m) where
  bindAt at b = mapWriterT (bindAt at b)
  matched t w = mapWriterT (matched t w)
  
instance (Monoid w, Read m) => Read (WriterT w m) where
  bindings = Trans.lift bindings
             
instance Monad m => Write (ReaderT [Bind] m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ _ = id
  
instance Monad m => Read (ReaderT [Bind] m) where 
  bindings = ask
  
instance Write m => Write (MaybeT m) where
  bindAt at b = mapMaybeT (bindAt at b)
  matched t w = mapMaybeT (matched t w)
  
instance Read m => Read (MaybeT m) where
  bindings = Trans.lift bindings
  
instance MatchRead m => MatchRead (MaybeT m) where
  matches = Trans.lift matches
  
instance Write m => Write (EitherT e m) where
  bindAt at b = mapEitherT (bindAt at b)
  matched t w = mapEitherT (matched t w)
  
instance Read m => Read (EitherT e m) where
  bindings = Trans.lift bindings

instance Write m => Write (StateT s m) where
  bindAt at b = mapStateT (bindAt at b)
  matched t w = mapStateT (matched t w)
  
instance Read m => Read (StateT s m) where
  bindings = Trans.lift bindings

instance Tracks r m => Tracks r (MaybeT m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapMaybeT (liftTrackedMany n)
  
instance (Monoid w, Tracks r m) => Tracks r (WriterT w m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapWriterT (liftTrackedMany n)
