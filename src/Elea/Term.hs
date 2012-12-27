module Elea.Term
(
  Term (..), Index,
  isInj, function,
  flattenApp, unflattenApp,
  liftAt, lift,
  substAt, substTop,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Type ( Type )

import qualified Elea.Type as Type

-- | De-bruijn indices for term variables
newtype Index = Index Int
  deriving ( Eq, Ord, Enum )

-- | System F with general recursion, inductive data-types and absurdity.
data Term
  = Var !Index
  | Inj !Int !Type
  | Abs !Type
  | App !Term !Term
  | Lam !Type !Term
  | Fix !Type !Term
  | Case { caseOfTerm :: !Term
         , caseOfAlts :: ![Term] }
  deriving ( Eq, Ord )
  
isInj :: Term -> Bool
isInj (Inj {}) = True
isInj _ = False

instance Uniplate Term where
  uniplate (Var idx) = 
    (Zero, \Zero -> Var idx)
  uniplate (Inj n ty) =
    (Zero, \Zero -> Inj n ty)
  uniplate (Abs ty) = 
    (Zero, \Zero -> Abs ty)
  uniplate (App t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> App t1 t2)
  uniplate (Lam ty rhs) = 
    (One rhs, \(One rhs) -> Lam ty rhs)
  uniplate (Fix ty rhs) =
    (One rhs, \(One rhs) -> Fix ty rhs)
  uniplate (Case lhs alts) = 
    (Two (One lhs) (listStr alts), 
      \(Two (One lhs) alts) -> Case lhs (strList alts))
      
flattenApp :: Term -> [Term]
flattenApp (App t1 t2) = t1 : flattenApp t2
      
unflattenApp :: [Term] -> Term
unflattenApp = foldr1 App 

function :: Term -> Term
function = head . flattenApp
 
-- | Substitute at the outermost De-bruijn index 0
substTop :: Term -> Term -> Term
substTop = substAt (toEnum 0)
      
-- | Substitute a given De-bruijn 'Index' with a 'Term' within a 'Term'
substAt :: Index -> Term -> Term -> Term
substAt at with = subst at
  where
  subst :: Index -> Term -> Term
  subst _ (Abs ty) = Abs ty
  subst _ (Inj n ty) = Inj n ty
  subst at (Var var) = 
    case at `compare` var of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred var)
      GT -> Var var
  subst at (t1 `App` t2) = 
    subst at t1 `App` subst at t2
  subst at (Lam ty rhs) =
    Lam ty (subst (succ at) rhs)
  subst at (Fix ty rhs) = 
    Fix ty (subst (succ at) rhs)
  subst at (Case lhs alts) = 
    Case (subst at lhs) (map (subst at) alts)

lift :: Term -> Term
lift = liftAt (toEnum 0)

liftAt :: Index -> Term -> Term
liftAt _ (Inj n ty) = Inj n ty
liftAt _ (Abs ty) = Abs ty
liftAt idx (Var x)
  | idx >= x = Var (succ x)
  | otherwise = Var x
liftAt idx (App t1 t2) = 
  App (liftAt idx t1) (liftAt idx t2)
liftAt idx (Lam ty rhs) = 
  Lam ty (liftAt (succ idx) rhs)
liftAt idx (Fix ty rhs) = 
  Fix ty (liftAt (succ idx) rhs)
liftAt idx (Case term alts) =
  Case (liftAt idx term) (map (liftAt idx) alts)

