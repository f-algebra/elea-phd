module Elea.Term.Constraint
(
  has, 
  to,
  fromMatch,
)
where

import Elea.Prelude
import Elea.Term
import qualified Elea.Type as Type
import qualified Elea.Term.Index as Indices


has :: Term -> Bool
has term
  | Case _ alts <- snd (flattenLam term) =
    length (filter (not . isBot . get altInner) alts) == 1
    
  | otherwise = False
  
  
to :: Term -> Constructor
to = id
  . get altConstructor
  . head
  . filter (not . isBot . get altInner)
  . caseAlts
  . snd
  . flattenLam
  
  
fromMatch :: (Type.HasType Term, Indexed Term)
  => Type -> (Term, Term) -> Term -> Term
fromMatch ret_ty (match_t, flattenApp -> Con con : _) term = 
  Case match_t (map mkAlt cs)
  where
  cs = Type.constructors (get Type.constructorOf con)
  
  mkAlt :: Constructor -> Alt
  mkAlt con' = 
    Alt con' bs alt_t
    where
    bs = Type.makeAltBindings con'
    alt_t
      | con' == con = Indices.liftMany (length bs) term
      | otherwise = Bot ret_ty
