-- | All things to do with our homeomorphic embedding termination check
module Elea.Embed 
(
  Code,
  Encodable (..),
  code,
)
where

import Elea.Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set
import qualified Data.Poset as Partial


newtype Code = Code { repr :: [Int] }
  deriving ( Show, Eq )

instance Monoid Code where
  mempty = Code []
  Code xs `mappend` Code ys = Code (xs ++ ys)
  
code :: [Int] -> Code
code = Code

-- | Things which can be checked for embedding
class Encodable a where
  encode :: a -> Code
  
instance Encodable Code where
  encode = id

-- | A partial well-order on codes
instance Partial.Ord Code where
  compare (Code x) (Code y) 
    | x == y = Partial.EQ
    | x `isSubsequenceOf` y = Partial.LT
    | y `isSubsequenceOf` x = Partial.GT
    | otherwise = Partial.NC
    

