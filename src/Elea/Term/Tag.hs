{-# LANGUAGE UndecidableInstances #-}
-- | Tags for indexed fixed-points. 
module Elea.Term.Tag
(
  Tag,
  Tagged (..),
  Gen (..),
  GenT,
  Has (..),
  exceptOmega,
  make,
  omega,
  null,
  replace,
  runGenT,
  uniqueId,
  with,
  tag
)
where

import Elea.Prelude hiding ( compare, map, null, get )
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
  = Tagged  { get :: !Tag
            , untag :: !a }
  deriving ( Functor, Foldable, Traversable )
        
mkLabels [ ''Tag ]
          
instance Eq Tag where
  (==) = (==) `on` _uniqueId
  
instance Ord Tag where
  compare = Prelude.compare `on` _uniqueId

instance Eq a => Eq (Tagged a) where
  (==) = (==) `on` untag
  
instance Ord a => Ord (Tagged a) where
  compare = Prelude.compare `on` untag
  
instance Show Tag where
  show t = "<" ++ (show . _uniqueId) t ++ ">"
  
class Monad m => Gen m where
  generateId :: m Int
  
make :: Gen m => m Tag
make = liftM Tag generateId

with :: Tag -> a -> Tagged a
with = Tagged

tag :: Gen m => a -> m (Tagged a)
tag x = do
  t <- make
  return (with t x)
  
class Has a where
  tags :: a -> Set Tag
  map :: (Tag -> Tag) -> a -> a
  
newtype GenT m a
  = GenT { genT :: StateT Int m a }
  deriving ( Functor, Applicative, Monad, MonadState Int, MonadTrans )
  
exceptOmega :: Has a => a -> Set Tag
exceptOmega = Set.delete omega . tags
  
replace :: Has a => Tag -> Tag -> a -> a
replace a b = map (\x -> if x == a then b else x)
  
runGenT :: Monad m => GenT m a -> m a
runGenT = flip evalStateT 1 . genT

omega :: Tag
omega = Tag (-2)

null :: Tag 
null = Tag 0

instance Has a => Has [a] where
  tags = Set.unions . Prelude.map tags
  map f = Prelude.map (map f)

  
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
  
instance Gen m => Gen (ReaderT r m) where
  generateId = Trans.lift generateId
  
instance Gen m => Gen (EitherT e m) where
  generateId = Trans.lift generateId
  

instance Show a => Show (Tagged a) where
  show (Tagged t x) = show x
  
  