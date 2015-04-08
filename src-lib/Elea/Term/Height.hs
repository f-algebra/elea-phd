module Elea.Term.Height
(
  Height (..),
  enforceDecrease
)
where

import Elea.Prelude
import Elea.Term
import Elea.Tag ( Tag )
import qualified Elea.Foldable as Fold 
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Embed as Embed
import qualified Elea.Tag as Tag
import qualified Data.Set as Set


data Height 
  = Height  { size :: !Nat
            , count :: !Nat }
            
            
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
    

height :: Term -> Height
height (Var _) = 1
height (Unr _) = 1
height (Con _) = 1
height (Lam _ t) = 1 + height t
height (Fix _ _ _) = 1 

height (Case t alts) = 
  heightCase t + concatMap (height . get altInner) alts
  where
  heightCase :: Term -> Height
  heightCase t@(leftmost -> Con _) = 1 + height t
  heightCase t@(Unr _) = 1 + height t
  heightCase t@(Case _ _) = 1 + height t
  heightCase t = height t
  
height (App f xs) = 
  heightApp f + concatMap heightApp xs
  where 
  heightApp :: Term -> Height
  heightApp t@(Case _ _) = 1 + height t
  heightApp t = height t
  
  
enforceDecrease :: Fail.Can m => (Term -> m Term) -> Term -> m Term
enforceDecrease f t = do
  t' <- f t
  let tag_order = Tag.order (tags t') (tags t)
  Fail.when (tag_order == GT)
  Fail.when (tag_order == EQ && height t' >= height t)
  return t'                              
  
