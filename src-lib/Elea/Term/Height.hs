module Elea.Term.Height
(
  Height (..),
  get,
  ensureDecrease,
  enforceDecrease,
  assertDecrease,
)
where

import Elea.Prelude hiding ( get )
import Elea.Term
import Elea.Term.Tag ( Tag )
import Elea.Show ()
import Elea.Term.Ext ()
import qualified Elea.Prelude as Pre
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold 
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Embed as Embed
import qualified Elea.Term.Tag as Tag
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Poset as Partial


data Height 
  = Height  { size :: !Nat
            , count :: !Nat }
            
instance Show Height where
  show (Height h n) = show (h, n)
            
instance Num Height where
  Height h1 n1 + Height h2 n2 = 
    Height (h1 + h2) (n1 * n2)  

  fromInteger n = Height (fromInteger n) 1
  
  _ * _ = undefined
  signum _ = undefined
  negate _ = undefined
  abs _ = undefined
    
  
instance Eq Height where
  h == h' = compare h h' == EQ
    
instance Ord Height where
  compare (Height h1 n1) (Height h2 n2)
    | h1 < h2 = LT
    | h2 < h1 = GT
    | n1 < n2 = LT
    | n2 < n1 = GT
    | otherwise = EQ
      
      
instance Monoid Height where
  mempty = Height 0 0
    
  Height h1 n1 `mappend` Height h2 n2 
    | h1 > h2 = Height h1 n1
    | h2 > h1 = Height h2 n2
    | otherwise = Height h1 (n1 + n2)
    

get :: Term -> Height
get (Var _) = 1
get (Unr _) = 1
get (Con _) = 1
get (Lam _ t) = 1 + get t
get (Fix inf b _) = 
  1 + fromIntegral arg_count
    + is_closed
  where
  arg_count = Type.argumentCount (Pre.get Type.bindType b)
  is_closed | Pre.get fixClosed inf = 0
            | otherwise = 1

get (Case t alts) = 
  1 + getCase t + concatMap (getAlt . Pre.get altInner) alts
  where
  getCase :: Term -> Height
  getCase t@(leftmost -> Con _) = 1 + get t
  getCase t@(Unr _) = 1 + get t
  getCase t@(Case _ _) = 1 + get t
  getCase t = get t
  
  getAlt (Lam _ t) = 2 + getAlt t
  getAlt t = get t
  
get (App f xs) = 
  getFun f + concatMap getApp xs
  where 
  getFun :: Term -> Height
  getFun t@(App _ _) = 1 + get t
  getFun t = getApp t
  
  getApp :: Term -> Height
  getApp t@(Case _ _) = 1 + get t
  getApp t = get t
  
-- | Our partial well-order on term size (tag ordering + height ordering)
instance Partial.Ord Term where
  compare t t' =
    Partial.compare (Tag.tags t) (Tag.tags t')
      ++  Partial.fromTotal (compare (get t) (get t'))
      -- ^ This monoid instance implements lexicographic ordering
    
      
ensureDecrease :: Fail.Can m => Term -> Term -> m ()
ensureDecrease t t' = 
  Fail.unless False -- (t Partial.< t')
      
      
assertDecrease :: Monad m => String -> (Term -> m Term) -> Term -> m Term
assertDecrease step f t = do
  t' <- f t
  if False || not (t' Partial.< t)
  then
    error $ "[" ++ step ++ "] failed to decrease: " 
         ++ show t ++ "\n\n==> into ==>\n" ++ show t'
  else
    return t'
  
  
enforceDecrease :: Fail.Can m => (Term -> m Term) -> Term -> m Term
enforceDecrease f t = do
  t' <- f t
  if False || not (t' Partial.< t)
  then do
    Fail.when (True || t == t')
    trace ("ENFORCE failed to decrease: " 
         ++ show t ++ "\n\nwith encoding: " ++ show (Embed.encode t)
         ++ "\n\n==> into ==>\n" 
         ++ show t' ++ "\n\nwith encoding: " ++ show (Embed.encode t')) 
         Fail.here
  else
    return t'
  
