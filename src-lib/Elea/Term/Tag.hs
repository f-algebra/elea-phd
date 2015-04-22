{-# LANGUAGE UndecidableInstances #-}
-- | Tags for indexed fixed-points. 
module Elea.Term.Tag
(
  Tag,
  Tagged (..),
  Gen (..),
  GenT,
  Has (..),
  make,
  omega,
  null,
  replace,
  runGenT,
  uniqueId,
  with
)
where

import Elea.Prelude hiding ( compare, map, null )
import Elea.Embed ( Encodable (..), Atom (..), cantor )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as StateT
import qualified Data.Set as Set
import qualified Data.Poset as Partial


newtype Tag 
  = Tag { _uniqueId :: Int }
  
data Tagged a
  = Tagged  { tag :: !Tag
            , tagged :: !a }
  deriving ( Functor, Foldable, Traversable )
        
mkLabels [ ''Tag ]
          
instance Eq Tag where
  (==) = (==) `on` _uniqueId
  
instance Ord Tag where
  compare = Prelude.compare `on` _uniqueId

instance Eq a => Eq (Tagged a) where
  (==) = (==) `on` tagged
  
instance Ord a => Ord (Tagged a) where
  compare = Prelude.compare `on` tagged
  
instance Show Tag where
  show = show . _uniqueId
  
class Monad m => Gen m where
  generateId :: m Int
  
make :: Gen m => m Tag
make = liftM Tag generateId

with :: Tag -> a -> Tagged a
with = Tagged
  
class Has a where
  tags :: a -> Set Tag
  map :: (Tag -> Tag) -> a -> a
  
newtype GenT m a
  = GenT { genT :: StateT Int m a }
  deriving ( Functor, Monad, MonadState Int, MonadTrans )
  
replace :: Has a => Tag -> Tag -> a -> a
replace a b = map (\x -> if x == a then b else x)
  
runGenT :: Monad m => GenT m a -> m a
runGenT = flip evalStateT 1 . genT

omega :: Tag
omega = Tag (-2)

null :: Tag 
null = Tag (-1)

  
instance Monad m => Gen (GenT m) where
  generateId = do
    id <- State.get
    State.put (id + 1)
    return id
      
instance MonadWriter w m => MonadWriter w (GenT m) where
  tell = Trans.lift . tell
  listen = GenT . listen . genT
  pass = GenT . pass . genT
  
instance (Monoid w, Gen m) => Gen (WriterT w m) where
  generateId = Trans.lift generateId
  
instance Gen m => Gen (MaybeT m) where
  generateId = Trans.lift generateId
  

instance Atom a => Atom (Tagged a) where
  atom (Tagged x t) = atom [11, atom x, atom t]
  
instance Atom Tag where
  atom (Tag x) = cantor (10, x)
  
  

instance Show a => Show (Tagged a) where
  show = show . tagged
  
