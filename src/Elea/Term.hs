module Elea.Term
(
  Term (..), Index,
  substAt, substTypeAt,
  substOutermost, substOutermostType,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type )

import qualified Elea.Type as Type

-- | De-bruijn indices for term variables
newtype Index = Index Int
  deriving ( Eq, Ord, Enum )

-- | System F terms with general recursion and least fixed point (un)folding.
data Term
  = Var !Index
  | App !Term !Term
  | Lam !Text !Type !Term
  | Fix !Text !Type !Term
  | TyApp !Term !Type
  | TyLam !Text !Term
  
  -- Folds least fixed points,
  -- given "Lfp F" is of type "F (Lfp F) -> Lfp F"
  | In !Type
  
  -- Unfolds least fixed points, 
  -- given "Lfp F" is of type "Lfp F -> F (Lfp F)"
  | Out !Type
  deriving ( Eq, Ord )
  
instance Uniplate Term where
  uniplate (Var idx) = 
    (Zero, \Zero -> Var idx)
  uniplate (App t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> App t1 t2)
  uniplate (Lam lbl ty rhs) = 
    (One rhs, \(One rhs) -> Lam lbl ty rhs)
  uniplate (Fix lbl ty rhs) =
    (One rhs, \(One rhs) -> Fix lbl ty rhs)
  uniplate (TyApp lhs ty) = 
    (One lhs, \(One lhs) -> TyApp lhs ty)
  uniplate (TyLam lbl rhs) =
    (One rhs, \(One rhs) -> TyLam lbl rhs)
  uniplate (In ty) =
    (Zero, \Zero -> In ty)
  uniplate (Out ty) =
    (Zero, \Zero -> Out ty)
    
-- | Substitute a given De-bruijn 'Index' with a 'Term' within a 'Term'
substAt :: Index -> Term -> Term -> Term
substAt at with = subst at
  where
  subst :: Index -> Term -> Term
  subst at (Var var) = 
    case at `compare` var of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred var)
      GT -> Var var
  subst at (t1 `App` t2) = 
    subst at t1 `App` subst at t2
  subst at (Lam lbl ty rhs) =
    Lam lbl ty (subst (succ at) rhs)
  subst at (Fix lbl ty rhs) = 
    Fix lbl ty (subst (succ at) rhs)
  subst at (TyApp lhs ty) = 
    TyApp (subst at lhs) ty
  subst at (TyLam lbl rhs) =
    TyLam lbl (subst at rhs)
  subst at (In ty) = In ty
  subst at (Out ty) = Out ty
  
-- | Substitute at the outermost De-bruijn index 0
substOutermost :: Term -> Term -> Term
substOutermost = substAt (toEnum 0)

-- | Substitute 'Type's within a 'Term'
substTypeAt :: Type.Index -> Type -> Term -> Term 
substTypeAt at with = subst at
  where
  substTy :: Type.Index -> Type -> Type
  substTy at = Type.substAt at with
  
  subst :: Type.Index -> Term -> Term
  subst at (Var var) = Var var
  subst at (t1 `App` t2) = 
    subst at t1 `App` subst at t2
  subst at (Lam lbl ty rhs) =
    Lam lbl (substTy at ty) (subst at rhs)
  subst at (Fix lbl ty rhs) = 
    Fix lbl (substTy at ty) (subst at rhs)
  subst at (TyApp lhs ty) = 
    TyApp (subst at lhs) (substTy at ty)
  subst at (TyLam lbl rhs) =
    TyLam lbl (subst (succ at) rhs)
  subst at (In ty) = 
    In (substTy at ty)
  subst at (Out ty) =
    Out (substTy at ty)
  
-- | Substitute the outermost De-bruijn indexed type variable within a 'Term'
substOutermostType :: Type -> Term -> Term
substOutermostType = substTypeAt (toEnum 0)
    
