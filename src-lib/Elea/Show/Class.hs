module Elea.Show.Class
(
  ShowM (..)
)
where

import Elea.Prelude
import qualified Data.Map as Map

-- | A monadic variant of 'show'
class Monad m => ShowM m a where
  showM :: a -> m String
  
instance ShowM m a => ShowM m (Set a) where
  showM xs = do
    xs_s <- showM (toList xs)
    return ("Set" ++ xs_s)
    
instance (ShowM m a, ShowM m b) => ShowM m (a, b) where
  showM (x, y) = do
    x_s <- showM x
    y_s <- showM y
    return ("(" ++ x_s ++ ", " ++ y_s ++ ")")
 
instance (ShowM m k, ShowM m a) => ShowM m (Map k a) where
  showM = showM . Map.toList
  
instance ShowM m a => ShowM m (Maybe a) where
  showM = liftM show . mapM showM
  
instance ShowM m a => ShowM m [a] where
  showM xs = do
    sxs <- mapM showM xs
    return 
      $ "[" ++ intercalate ", " sxs ++ "]"

