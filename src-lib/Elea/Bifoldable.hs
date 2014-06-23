module Elea.Bifoldable
(
  Bifoldable (..), 
  map, mapM
)
where

import Elea.Prelude hiding ( map, mapM )
import Elea.Foldable ( Base )
import qualified Elea.Foldable as Fold

-- | Bifoldable represents types which contain another type.
-- The type 'Inner a' is the type contained in 'a', and 'Base a' is
-- a type function which exposes the inner type. 
-- Gives an isomorphism between 'a' and 'Base a (Inner a)'.
-- > project . embed = id
-- > embed . project = id
class Bifoldable a where
  type Inner a
  project :: a -> Base a (Inner a)
  embed :: Base a (Inner a) -> a


map :: (Functor (Base a), Bifoldable a) => (Inner a -> Inner a) -> a -> a
map f = embed . fmap f . project

mapM :: (Traversable (Base a), Bifoldable a, Monad m)
  => (Inner a -> m (Inner a)) -> a -> m a
mapM f = liftM embed . sequence . fmap f . project


