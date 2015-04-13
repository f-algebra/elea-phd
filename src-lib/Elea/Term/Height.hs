module Elea.Term.Height
(
  Height (..),
  enforceDecrease,
  assertDecrease,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Tag ( Tag )
import Elea.Show ()
import Elea.Terms ()
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold 
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Embed as Embed
import qualified Elea.Tag as Tag
import qualified Data.Set as Set
import qualified Data.Map as Map


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
height (Fix inf b _) = 
  1 + fromIntegral arg_count
    + is_closed
  where
  arg_count = Type.argumentCount (get Type.bindType b)
  is_closed | get fixClosed inf = 0
            | otherwise = 1

height (Case t alts) = 
  heightCase t + concatMap (heightAlt . get altInner) alts
  where
  heightCase :: Term -> Height
  heightCase t@(leftmost -> Con _) = 1 + height t
  heightCase t@(Unr _) = 1 + height t
  heightCase t@(Case _ _) = 1 + height t
  heightCase t = height t
  
  heightAlt (Lam _ t) = 2 + heightAlt t
  heightAlt t = height t
  
height (App f xs) = 
  heightFun f + concatMap heightApp xs
  where 
  heightFun :: Term -> Height
  heightFun t@(App _ _) = 1 + height t
  heightFun t = heightApp t
  
  heightApp :: Term -> Height
  heightApp t@(Case _ _) = 1 + height t
  heightApp t = height t
  
  
assertDecrease :: Monad m => String -> (Term -> m Term) -> Term -> m Term
assertDecrease step f t = do
  t' <- f t
  let tag_order = Tag.compare t' t
  if tag_order == GT
    || (tag_order == EQ && height t' >= height t)
  then
    error $ "[" ++ step ++ "] failed to decrease: " 
         ++ show t ++ "\n\n==> into ==>\n" ++ show t'
  else
    return t'
  
  
enforceDecrease :: Fail.Can m => (Term -> m Term) -> Term -> m Term
enforceDecrease f t = do
  t' <- f t
  let tag_order = Tag.compare t' t
  if tag_order == GT
    || (tag_order == EQ && height t' >= height t)
  then do
    Fail.when (True || t == t')
    trace ("ENFORCE failed to decrease: " 
         ++ show t ++ "\n\nwith encoding: " ++ show (Embed.encode t)
         ++ "\n\n==> into ==>\n" 
         ++ show t' ++ "\n\nwith encoding: " ++ show (Embed.encode t')) 
         Fail.here
  else
    return t'
  
