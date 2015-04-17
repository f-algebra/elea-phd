-- | All things to do with our homeomorphic embedding termination check
module Elea.Embed 
(
  Code,
  Encodable (..),
  code,
  encode,
  (<=)
)
where

import Elea.Prelude hiding ( (<=) )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set
import qualified Data.Poset as Partial

-- We store the length to speed up embedding checks
data Code 
  = Code { len :: !Int
         , repr :: ![Int] }
  deriving ( Show, Eq )

instance Monoid Code where
  mempty = code []
  Code n xs `mappend` Code m ys = Code (n + m) (xs ++ ys)
  
code :: [Int] -> Code
code xs = Code (length xs) xs

-- | Things which can be checked for embedding
class Encodable a where
  encodeRaw :: a -> [Int]
  
encode :: Encodable a => a -> Code
encode = code . encodeRaw

-- | Flattened homeomorphic embedding 
(<=) :: Encodable a => a -> a -> Bool
(<=) = (Partial.<=) `on` encode
  

-- | A partial well-order on codes
instance Partial.Ord Code where
  compare (Code n xs) (Code m ys) 
    | n == m, xs == ys = Partial.EQ
    | n Prelude.<= m, xs `isSubsequenceOf` ys = Partial.LT
    | m Prelude.<= n, ys `isSubsequenceOf` xs = Partial.GT
    | otherwise = Partial.NC
    
  Code n xs <= Code m ys =
    n Prelude.<= m && xs `isSubsequenceOf` ys

