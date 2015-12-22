module Elea.Term.Constraint
(
  has, 
  to,
  fromCase,
  splittable,
  split,
  apply,
  applyAll,
  removeWhen,
  removeAll,
)
where

import Elea.Prelude hiding ( removeAll )
import Elea.Term hiding ( constructor )
import qualified Elea.Prelude as Prelude
import qualified Elea.Monad.Env as Env
import qualified Elea.Term as Term
import qualified Elea.Type as Type
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set


has :: Term -> Bool
has term
  | Case _ alts <- snd (flattenLam term) =
    let (bot, not_bot) = partition (isBot . get altInner) alts in
    length bot >= 1 && length not_bot == 1
    
  | otherwise = False
  
  
to :: Term -> Tagged Constructor
to = id
  . get altConstructor
  . head
  . filter (not . isBot . get altInner)
  . caseAlts
  . snd
  . flattenLam

  
fromCase :: Term -> Constraint
fromCase (Case cse_t alts) = 
  Match cse_t pats (enum alt_i)
  where
  [alt_i] = findIndices (not . isBot . get altInner) alts
  pats = zipWith stripVars [0..] (map altPattern alts)
  
  stripVars :: Int -> Pattern -> Pattern
  stripVars i p@(Pattern tc bs _)
    | i == enum alt_i = p
    | otherwise = Pattern tc bs []
  
splittable :: Term -> Bool
splittable term@(Case _ alts)
  | length non_abs == 1 
  , length abs >= 1 =
    lowerableAltInner (head non_abs)
  where
  (non_abs, abs) = partition (not . isBot . get altInner) alts
  
splittable _ = False


split :: Term -> (Constraint, Term, Type)
split term@(Case _ alts) = 
  ( fromCase term
  , loweredAltInner (alts !! alt_i) 
  , res_ty )
  where
  [alt_i] = findIndices (not . isBot . get altInner) alts
  Just (Alt _ _ (Bot res_ty)) = find (isBot . get altInner) alts

  
applyAll :: Type -> Set Constraint -> Term -> Term
applyAll ret_ty cts term =
  foldr (apply ret_ty) term (Set.toAscList cts)
  
  
apply :: Type -> Constraint -> Term -> Term
apply ret_ty match term = 
  Case (matchedTerm match) 
       (zipWith mkAlt [0..] (get matchPatterns match))
  where
  idx = get matchIndex match
  
  mkAlt :: Nat -> Pattern -> Alt
  mkAlt n (Pattern tcon bs _) =
    Alt tcon bs alt_t
    where
    alt_t
      | n == idx = Indices.liftMany (nlength bs) term
      | otherwise = Bot ret_ty

removeWhen :: (Constraint -> Term -> Bool) -> Term -> Term
removeWhen when = Fold.transform remove
  where
  remove term
    | splittable term
    , when ct term' = term'
    | otherwise = term
    where
    (ct, term', _) = split term
      
removeAll :: Term -> Term
removeAll = removeWhen (\_ _ -> True)
