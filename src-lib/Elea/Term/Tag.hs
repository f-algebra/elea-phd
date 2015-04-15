{-# LANGUAGE UndecidableInstances #-}
-- | Tags for indexed fixed-points. 
module Elea.Term.Tag
(
  Tag,
  Gen (..),
  GenT,
  Has (..),
  make,
  omega,
  replace,
  runGenT,
  uniqueId,
  smallerIndices,
)
where

import Elea.Prelude hiding ( compare, map )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as StateT
import qualified Data.Set as Set
import qualified Data.Poset as Partial


data Tag 
  = Tag { _uniqueId :: !Int
        , _smallerIndices :: !(Set Tag) }
        
mkLabels [ ''Tag ]
          
instance Eq Tag where
  (==) = (==) `on` _uniqueId
  
instance Ord Tag where
  compare = Prelude.compare `on` _uniqueId
  
instance Show Tag where
  show = show . _uniqueId
  
class Monad m => Gen m where
  generateId :: m Int
  
make :: Gen m => Set Tag -> m Tag
make smaller_tags = do
  new_id <- generateId
  return (Tag new_id all_smaller)
  where
  all_smaller = id
    . Set.unions 
    . Prelude.map collapse 
    $ Set.toList smaller_tags
    where
    collapse tag@(Tag _ ids) = Set.insert tag ids
  
class Has a where
  tags :: a -> Set Tag
  map :: (Tag -> Tag) -> a -> a
  
  compareTags :: a -> a -> Partial.Ordering
  compareTags = Partial.compare `on` tags
  
-- | A partial well-order on sets of tags
instance Partial.Ord (Set Tag) where
  compare a b 
    | a == b = Partial.EQ 
    
    | not (Set.null a_minus_b)
    , allSmaller (smallerIn a) b_minus_a =
      Partial.GT
      
    | not (Set.null b_minus_a)
    , allSmaller (smallerIn b) a_minus_b =
      Partial.LT
      
    | otherwise = Partial.NC
    where
    -- All tags which have a larger tag in this set   
    smallerIn :: Set Tag -> Set Tag 
    smallerIn = id
      . Set.unions 
      . Prelude.map (get smallerIndices) 
      . Set.toList
    
    allSmaller :: Set Tag -> Set Tag -> Bool
    allSmaller xs =
      all (\tag -> Set.member tag xs) . Set.toList
      
    a_minus_b = Set.difference a b
    b_minus_a = Set.difference b a
      
  
newtype GenT m a
  = GenT { genT :: StateT Int m a }
  deriving ( Functor, Monad, MonadState Int, MonadTrans )
  
replace :: Has a => Tag -> Tag -> a -> a
replace a b = map (\x -> if x == a then b else x)
  
runGenT :: Monad m => GenT m a -> m a
runGenT = flip evalStateT 1 . genT

omega :: Tag
omega = Tag 0 Set.empty

  
instance Monad m => Gen (GenT m) where
  generateId = do
    id <- State.get
    State.put (id + 1)
    return id
      
instance MonadWriter w m => MonadWriter w (GenT m) where
  tell = Trans.lift . tell
  listen = GenT . listen . genT
  pass = GenT . pass . genT
  
instance Gen m => Gen (MaybeT m) where
  generateId = Trans.lift generateId
  
