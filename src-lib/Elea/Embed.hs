-- | All things to do with our homeomorphic embedding termination check
module Elea.Embed 
(
  Encodable (..),
  History (..),
  HistoryT,
  emptyHistoryT,
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Tag as Tag
import qualified Elea.Monad.Eval as Eval
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Set as Set


newtype Code = Code { repr :: [Int] }

instance Monoid Code where
  mempty = Code []
  Code xs `mappend` Code ys = Code (xs ++ ys)

-- | Things which can be checked for embedding
class Encodable a where
  encode :: a -> Code
  
instance Encodable Code where
  encode = id

class Fail.Can m => History m where
  seen :: Encodable a => a -> m b -> m b
  
newtype HistoryT m a
  = HistoryT { historyT :: ReaderT [Code] m a }
  deriving ( Functor, Monad, MonadReader [Code], MonadTrans )

emptyHistoryT :: HistoryT m a -> m a
emptyHistoryT = flip runReaderT mempty  . historyT

mapHistoryT :: (m a -> n b) -> HistoryT m a -> HistoryT n b
mapHistoryT f = HistoryT . mapReaderT f . historyT

  
instance Fail.Can m => History (HistoryT m) where
  seen (encode -> code) continue = do
    prev_codes <- ask
    Fail.when (any (code `embedsInto`) prev_codes)
    local (\codes -> code:codes) continue
    
instance Fail.Can m => Fail.Can (HistoryT m) where
  here = Trans.lift Fail.here
  catch = mapHistoryT Fail.catch
  
instance History m => History (MaybeT m) where
  seen a = mapMaybeT (seen a)
  
instance History m => History (Eval.RuleT m) where
  seen a = Eval.mapRuleT (seen a)
  

embedsInto :: (Encodable a, Encodable b) => a -> b -> Bool
embedsInto (encode -> Code x) (encode -> Code y) = 
  x `isSubsequenceOf` y
  
     
cantor :: Int -> Int -> Int
cantor m n =
  (m + n) * (m + n + 1) `quot` 2 + m
  
pair :: Int -> Int -> Code
pair x y = Code [cantor x y]

instance Encodable Term where
  encode (Unr _) = pair 1 0
  encode (Var _) = pair 2 0
  encode (Lam _ t) = pair 3 0 ++ encode t
  encode (Con con) = encode con
  encode (App f xs) =
    encode f ++ concatMap ((pair 4 0 ++) . encode) xs
  encode (Fix inf _ _) = 
    pair 5 tag_id
    where
    tag_id = (get Tag.uniqueId . get fixTag) inf
  encode (Case cse_t alts) = 
    encode cse_t ++ concatMap encode alts
    
instance Encodable Constructor where
  encode (Constructor ind idx) = 
    pair 6 (cantor (hash ind) (enum idx))
    
instance Encodable Alt where
  encode (Alt con bs t) = 
    pair 7 0 ++ encode t
