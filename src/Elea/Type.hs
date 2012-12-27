module Elea.Type 
(
  Type (..), Index,
  substAt, substOutermost,
  unflatten, flatten
)
where

import Prelude ()
import Elea.Prelude 

-- | De-bruijn indices for type variables
newtype Index = Index Int
  deriving ( Eq, Ord, Enum )

data Type
  = Var !Index
  | Arr !Type !Type
  | Ind ![[Type]]
  deriving ( Eq, Ord )
  
unflatten :: [Type] -> Type
unflatten = foldl1 Arr

flatten :: Type -> [Type]
flatten (Arr t1 t2) = t1 : flatten t2

instance Uniplate Type where
  uniplate (Var idx) =
    (Zero, \Zero -> Var idx)
  uniplate (Arr ty1 ty2) = 
    (Two (One ty1) (One ty2), \(Two (One ty1) (One ty2)) -> Arr ty1 ty2)
  uniplate (Ind sum) =
    (listStr (concat sum), 
      \args -> Ind (splitArgs argCounts (strList args)))
    where
    argCounts = map length sum
    
    splitArgs [] [] = []
    splitArgs (len:lens) all = 
      [tke] ++ splitArgs lens drp
      where
      (tke, drp) = splitAt len all

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
  subst at (Ind args) =
    Ind (map (map (subst (succ at))) args)
    
-- | Substitute the first De-bruijn index with a 'Type' within a 'Type'
substOutermost :: Type -> Type -> Type
substOutermost = substAt (toEnum 0)


