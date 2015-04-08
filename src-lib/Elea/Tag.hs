module Elea.Tag
(
  Tag,
  Gen (..),
  TagGenT,
  omega,
  runGen,
  uniqueId,
  smallerIndices,
  order,
)
where

import Elea.Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.State.Class as State
import qualified Data.Set as Set


data Tag 
  = Tag { _uniqueId :: !Int
        , _smallerIndices :: !(Set Tag) }
        
mkLabels [ ''Tag ]
          
instance Eq Tag where
  (==) = (==) `on` _uniqueId
  
instance Ord Tag where
  compare = compare `on` _uniqueId
  
class Monad m => Gen m where
  make :: [Tag] -> m Tag
  
newtype TagGenT m a
  = TagGenT { tagGenT :: StateT Int m a }
  deriving ( Functor, Monad, MonadState Int, MonadTrans )
  
runGen :: Monad m => TagGenT m a -> m a
runGen = flip evalStateT 1 . tagGenT

omega :: Tag
omega = Tag 0 Set.empty

-- | A partial well-order on sets of tags. 'EQ' means incomparable.
order :: Set Tag -> Set Tag -> Ordering
order a b =
  if not (Set.null a_minus_b)
    && allSmaller (smallerIn a) b_minus_a
  then GT
  else if not (Set.null b_minus_a)
    && allSmaller (smallerIn b) a_minus_b
  then LT
  else EQ
  where
  -- All tags which have a larger tag in this set   
  smallerIn :: Set Tag -> Set Tag 
  smallerIn = id
    . Set.unions 
    . map (get smallerIndices) 
    . Set.toList
  
  allSmaller :: Set Tag -> Set Tag -> Bool
  allSmaller xs =
    all (\tag -> Set.member tag xs) . Set.toList
    
  a_minus_b = Set.difference a b
  b_minus_a = Set.difference b a

  
instance Monad m => Gen (TagGenT m) where
  make smaller_tags = do
    next_id <- State.get
    State.put (succ next_id)
    return (Tag next_id all_smaller)
    where
    all_smaller = Set.unions (map collapse smaller_tags)
      where
      collapse tag@(Tag _ ids) = Set.insert tag ids
  
