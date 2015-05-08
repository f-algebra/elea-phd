module Elea.Term.Constraint
(
  has, 
  to,
  add,
  apply,
  alreadyFused,
  subsume,
  forget,
)
where

import Elea.Prelude
import Elea.Term hiding ( constructor )
import qualified Elea.Monad.Env as Env
import qualified Elea.Term as Term
import qualified Elea.Type as Type
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Data.Set as Set



has :: Term -> Bool
has term
  | Case _ alts <- snd (flattenLam term) =
    length (filter (not . isBot . get altInner) alts) == 1
    
  | otherwise = False
  
  
to :: Term -> Tagged Constructor
to = id
  . get altConstructor
  . head
  . filter (not . isBot . get altInner)
  . caseAlts
  . snd
  . flattenLam
  
  
alreadyFused :: Constraint -> Term -> Bool
alreadyFused ct = id
  . Set.member ct
  . get fixDomain
  . fixInfo
  
add :: Constraint -> Term -> Term
add ct (Fix fix_i fix_b fix_t) = 
  Fix (modify fixDomain (Set.insert ct) fix_i) fix_b fix_t
 

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
      
      
subsume :: [Constraint] -> [Constraint]
subsume (Set.fromList -> cts) = 
  Set.toList (cts Set.\\ subsumed)
  where
  subsumed :: Set Constraint
  subsumed = id
    . Set.unions 
    . map innerCt
    $ Set.toList cts
  
  innerCt :: Constraint -> Set Constraint
  innerCt (matchedTerm -> App (Fix fix_i _ _) _) =
    get fixDomain fix_i
    
    
forget :: Env.Write m => FixInfo -> m a -> m a
forget inf = Env.forgetMatches (`Set.member` get fixDomain inf)

