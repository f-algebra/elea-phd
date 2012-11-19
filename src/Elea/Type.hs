module Elea.Type 
(
  Type (..), Index,
  substAt, substOutermost, 
  unfoldLfp
)
where

import Prelude ()
import Elea.Prelude hiding ( All )

-- | De-bruijn indices for type variables
newtype Index = Index Int
  deriving ( Eq, Ord, Enum )

-- | System F types with least fixed points
data Type  
  = Var !Index
  | Arr !Type !Type
  | All !Text !Type
  | Lfp !Text !Type
  deriving ( Eq, Ord )
  
instance Uniplate Type where
  uniplate (Var idx) =
    (Zero, \Zero -> Var idx)
  uniplate (Arr ty1 ty2) = 
    (Two (One ty1) (One ty2), \(Two (One ty1) (One ty2)) -> Arr ty1 ty2)
  uniplate (All lbl ty) = 
    (One ty, \(One ty) -> All lbl ty)
  uniplate (Lfp lbl ty) =
    (One ty, \(One ty) -> Lfp lbl ty)

-- | Substitute a given De-bruijn 'Index' with a 'Type' within a 'Type'
substAt :: Index -> Type -> Type -> Type
substAt at with = subst at
  where
  subst at (Var var) = 
    case at `compare` var of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred var)
      GT -> Var var
  subst at (t1 `Arr` t2) = 
    subst at t1 `Arr` subst at t2
  subst at (All lbl t) =
    All lbl (subst (succ at) t)
  subst at (Lfp lbl t) =
    Lfp lbl (subst (succ at) t)
    
-- | Substitute the first De-bruijn index with a 'Type' within a 'Type'
substOutermost :: Type -> Type -> Type
substOutermost = substAt (toEnum 0)

-- | Takes an "Lfp F" and returns "F (Lfp F)"
unfoldLfp :: Type -> Type
unfoldLfp mu@(Lfp _ ty) = substOutermost mu ty

