{-# LANGUAGE UndecidableInstances #-}
-- | Type and pattern match environment monad constraints
module Elea.Monad.Env.Class
(
  Write (..), Read (..), All,
  bind, bindMany, isBound,
  boundAt, bindingDepth,
  bindBranch,
  forgetMatch,
  
  MatchRead (..), 
  findMatches,
  findMatch,
  forgetAllMatches,
  findConstraints,
  isMatched,
  
  Tracks (..), 
  trackeds, liftTracked, 
  offset, liftByOffset, 
  lowerByOffset, lowerableByOffset,
  tryLowerByOffset,
)
where

import Elea.Prelude hiding ( Read (..) )
import Elea.Term.Index
import Elea.Term
import Elea.Unification ( Unifier )
import qualified Elea.Type as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.History as History
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map

type All m = (Read m, MatchRead m)

-- | A writable type and pattern match environment.
class Monad m => Write m where
  -- | Bind a variable index to a type within the environment
  bindAt :: Index -> Bind -> m a -> m a
  
  -- | Declare that the first term has been pattern matched to the second
  matched :: Match -> m a -> m a
  
  forgetMatches :: (Match -> Bool) -> m a -> m a
  
forgetMatch :: Write m => Match -> m a -> m a
forgetMatch m = forgetMatches (== m)

forgetAllMatches :: Write m => m a -> m a
forgetAllMatches = forgetMatches (const True)
  
bind :: Write m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Write m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

bindBranch :: (Indexed Term, Write m) => Term -> Nat -> m a -> m a
bindBranch cse_of@(Case _ alts) branch_n = id
  . bindMany binds 
  . matched (matchFromCase branch_n cse_of)
  where
  Alt { _altBindings = binds } = alts !! branch_n


-- | Whether you can read variable bindings from the environment. 
-- Since variables are indices, it's really a lookup function 'Nat -> Bind',
-- but this is just a list.
class Write m => Read m where
  bindings :: m [Bind]
  
-- | Lookup the type of a variable. 
boundAt :: Read m => Index -> m Bind
boundAt at = do
  bs <- bindings
  if at >= elength bs
  then error $ "Cannot retrieve the binding for index " ++ show at
    ++ " in an environment with only " ++ show (length bs) 
    ++ " bindings."
  else return (nth bs (enum at))
  
-- | Returns the number of indices that have been bound.
bindingDepth :: Read m => m Nat
bindingDepth = liftM nlength bindings

-- | Is a given index bound within this environment.
isBound :: Read m => Index -> m Bool
isBound at = do
  bs <- bindings
  return (at < elength bs)

-- | Whether you can read locally bound pattern matches from
-- an environment monad
class Write m => MatchRead m where
  matches :: m [Match]
  
findMatches :: MatchRead m => (Match -> Bool) -> m [Match]
findMatches p = liftM (filter p) matches

findMatch :: (Fail.Can m, MatchRead m) => Term -> m Term
findMatch t = do
  ms <- findMatches ((== t) . get matchTerm) 
  Fail.when (null ms)
  return (matchedTo (head ms))

findConstraints :: MatchRead m => (Constraint -> Bool) -> m [Constraint]
findConstraints when = 
  findMatches keep  
  where
  keep match = True
    && (isFix . leftmost . get matchTerm) match
    && when match

  
isMatched :: MatchRead m => Term -> m Bool
isMatched = id
  . liftM isJust 
  . runMaybeT
  . findMatch
 

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

tryLowerByOffset :: (Enum e, Tracks e m, Indexed a, Fail.Can m) => a -> m a
tryLowerByOffset x = do
  n <- offset
  Indices.tryLowerMany n x 

lowerableByOffset :: (Enum e, Tracks e m, Indexed a) => a -> m Bool
lowerableByOffset x = liftM (flip Indices.lowerableBy x) offset 


-- * Various generic instances for these type classes

instance Write Identity where
  bindAt _ _ = id
  matched _ = id
  forgetMatches _ = id

instance Monad m => Write (IdentityT m) where
  -- The 'IdentityT' monad just ignores all the written type information
  bindAt _ _ = id
  matched _ = id
  forgetMatches _ = id

instance (Monoid w, Write m) => Write (WriterT w m) where
  bindAt at b = mapWriterT (bindAt at b)
  matched m = mapWriterT (matched m)
  forgetMatches w = mapWriterT (forgetMatches w)
  
instance (Monoid w, Read m) => Read (WriterT w m) where
  bindings = Trans.lift bindings
             
instance Monad m => Write (ReaderT [Bind] m) where
  bindAt at b = local (insertAt (enum at) b)
  matched _ = id
  forgetMatches _ = id
  
instance Monad m => Read (ReaderT [Bind] m) where 
  bindings = ask
  
instance Write m => Write (MaybeT m) where
  bindAt at b = mapMaybeT (bindAt at b)
  matched m = mapMaybeT (matched m)
  forgetMatches w = mapMaybeT (forgetMatches w)
  
instance Read m => Read (MaybeT m) where
  bindings = Trans.lift bindings
  
instance MatchRead m => MatchRead (MaybeT m) where
  matches = Trans.lift matches
  
instance Write m => Write (EitherT e m) where
  bindAt at b = mapEitherT (bindAt at b)
  matched m = mapEitherT (matched m)
  forgetMatches w = mapEitherT (forgetMatches w)
  
instance Read m => Read (EitherT e m) where
  bindings = Trans.lift bindings

instance Write m => Write (StateT s m) where
  bindAt at b = mapStateT (bindAt at b)
  matched m = mapStateT (matched m)
  forgetMatches w = mapStateT (forgetMatches w)
  
instance Read m => Read (StateT s m) where
  bindings = Trans.lift bindings

instance Tracks r m => Tracks r (MaybeT m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapMaybeT (liftTrackedMany n)
  
instance (Monoid w, Tracks r m) => Tracks r (WriterT w m) where
  tracked = Trans.lift tracked
  liftTrackedMany n = mapWriterT (liftTrackedMany n)
  

instance Read m => Read (History.EnvT m) where
  bindings = Trans.lift bindings

instance Write m => Write (History.EnvT m) where
  bindAt at b = History.mapEnvT (bindAt at b)
  matched m = History.mapEnvT (matched m)
  forgetMatches w = History.mapEnvT (forgetMatches w)
  
  
instance MatchRead m => MatchRead (History.EnvT m) where
  matches = Trans.lift matches
