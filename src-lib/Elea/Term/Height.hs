module Elea.Term.Height
(
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
import qualified Elea.Term.Index as Indices
import qualified Elea.Prelude as Pre
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold 
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Embed as Embed
import qualified Elea.Term.Tag as Tag
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Poset as Partial


get :: Term -> Nat
get (Var _) = 1
get (Eql x y) = 1 + get x + get y
get (Bot _) = 1
get (Con _) = 1
get (Lam _ t) = 1 + get t
get (Fix inf b t) = 1 + get t

get (Case t alts) = 
  1 + getCase t + maximum (map (getAlt . Pre.get altInner) alts)
  where
  getCase :: Term -> Nat
  getCase t@(leftmost -> Con _) = 1 + get t
  getCase t@(Case _ _) = 1 + get t
  getCase t = get t
  
  getAlt :: Term -> Nat
  getAlt (Lam _ t) = 2 + getAlt t
  getAlt t = get t
    {-
get (App f@(Fix _ _ f_t) xs) = 1     
  + length (screen isRep xs)
  + length (filter (not . isVar) xs)
  + maximum (map get xs)
  where
  isRep :: [Term] -> Term -> [Term] -> Bool
  isRep left_xs (Var x) right_xs =
    x `Indices.freeWithin` free_elsewhere
    where
    free_elsewhere = 
      Indices.free f ++ Indices.free left_xs ++ Indices.free right_xs
  isRep _ _ _  = False
  -}
  
get (App f xs) = 
  getFun f 
    + maximum (map getApp xs) 
   --  + overlaps
   -- + repetitions
  where 
  getFun :: Term -> Nat
  getFun t@(App _ _) = 1 + get t
  getFun t = getApp t
  
  getApp :: Term -> Nat
  getApp t@(Case _ _) = 1 + get t
  getApp t = get t
  
  overlaps :: Nat
  overlaps = id
    . enum
    . Set.size 
    $ Set.intersection (Indices.free f) (Indices.free xs)
    
  repetitions :: Nat 
  repetitions = id
    . enum
    . Set.size
    . Set.filter (\xs -> length xs > 1)
    . Set.map (\x -> filter (Set.member x . Indices.free) xs)
    $ Indices.free xs
    
  
-- | Our partial well-order on term size (tag ordering + height ordering)
instance Partial.Ord Term where
  compare t t' =
    Partial.fromTotal (compare (get t) (get t'))
      -- ^ This monoid instance implements lexicographic ordering
    
      
ensureDecrease :: Fail.Can m => Term -> Term -> m ()
ensureDecrease t t' = 
  Fail.unless (get t < get t')
      
      
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
  
