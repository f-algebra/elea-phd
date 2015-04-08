module Elea.Tag
(
  Tag,
  Gen (..),
  TagGenT,
  omega,
  runGen,
  uniqueId,
)
where

import Elea.Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.State.Class as State
import qualified Data.Set as Set


data Tag 
  = Tag { _uniqueId :: !Int
        , _smallerIndices :: !(Set Int) }
        
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
  
instance Monad m => Gen (TagGenT m) where
  make smaller_tags = do
    next_id <- State.get
    State.put (succ next_id)
    return (Tag next_id all_smaller)
    where
    all_smaller = Set.unions (map collapse smaller_tags)
      where
      collapse (Tag id ids) = Set.insert id ids
  
