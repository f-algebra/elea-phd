module Elea.Term
(
  Term (..), Index, Alt (..),
  isInj, leftmost,
  flattenApp, unflattenApp,
  unflattenLam,
  liftAt, lift,
  substAt, substTop,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )

-- | De-bruijn indices for term variables
newtype Index = Index Int
  deriving ( Eq, Ord )

-- | The untyped, De-bruijn indexed lambda calculus,
-- with general recursion ('Fix'), 
-- inductive data types ('Inj' and 'Case'),
-- and absurdity ('Absurd').
data Term
  = Var !Index
  | App !Term !Term
  | Lam !Term
  | Fix !Term
  | Inj !Int
  | Case !Term ![Alt]
  | Absurd
  deriving ( Eq, Ord )
  
data Alt 
  = Alt { altVars :: !Int
        , altTerm :: !Term }
  deriving ( Eq, Ord )
  
isInj :: Term -> Bool
isInj (Inj {}) = True
isInj _ = False
      
flattenApp :: Term -> [Term]
flattenApp (App t1 t2) = t1 : flattenApp t2
      
unflattenApp :: [Term] -> Term
unflattenApp = foldr1 App 

unflattenLam :: Int -> Term -> Term
unflattenLam 0 t = t
unflattenLam n t 
  | n > 0 = Lam (unflattenLam (n - 1) t)

-- | Returns the leftmost 'Term' in term application.
-- E.g. "leftmost (App (App a b) c) == a"
leftmost :: Term -> Term
leftmost = head . flattenApp
 
-- | Substitute at the outermost De-bruijn index 0
substTop :: Term -> Term -> Term
substTop = substAt (toEnum 0)
      
-- | Substitute a given De-bruijn 'Index' with a 'Term' within a 'Term'
substAt :: Index -> Term -> Term -> Term
substAt at with = subst at
  where
  subst :: Index -> Term -> Term
  subst _ Absurd = Absurd
  subst _ (Inj n) = Inj n
  subst at (Var var) = 
    case at `compare` var of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred var)
      GT -> Var var
  subst at (t1 `App` t2) = 
    subst at t1 `App` subst at t2
  subst at (Lam rhs) =
    Lam (subst (succ at) rhs)
  subst at (Fix rhs) = 
    Fix (subst (succ at) rhs)
  subst at (Case lhs alts) = 
    Case (subst at lhs) (map substAlt alts)
    where
    substAlt (Alt n t) = Alt n (subst (at `plus` n) t)

-- | Increments every free De-Bruijn index in a 'Term' by one.
-- Equivalent to creating a De-Bruijn index at 0 - see 'liftAt'.
lift :: Term -> Term
lift = liftAt (toEnum 0)

-- | Creates a new De-Bruijn index 
-- by shifting all the later ones up by one (see the 'Var' case).
liftAt :: Index -> Term -> Term
liftAt at (Var x)
  | at >= x = Var (succ x)
  | otherwise = Var x
liftAt _ (Inj n) = Inj n
liftAt _ Absurd = Absurd
liftAt at (App t1 t2) = 
  App (liftAt at t1) (liftAt at t2)
liftAt at (Lam rhs) = 
  Lam (liftAt (succ at) rhs)
liftAt at (Fix rhs) = 
  Fix (liftAt (succ at) rhs)
liftAt at (Case lhs alts) =
  Case (liftAt at lhs) (map liftAlt alts)
  where 
  liftAlt (Alt n t) = Alt n (liftAt (at `plus` n) t)

instance Enum Index where
  succ (Index n) = Index (n + 1)
  pred (Index n) | n > 0 = Index (n - 1)
  toEnum n | n >= 0 = Index n
  fromEnum (Index n) = n
  
instance Uniplate Term where
  uniplate (Var idx) = 
    (Zero, \Zero -> Var idx)
  uniplate (Inj n) =
    (Zero, \Zero -> Inj n)
  uniplate Absurd = 
    (Zero, \Zero -> Absurd)
  uniplate (App t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> App t1 t2)
  uniplate (Lam rhs) = 
    (One rhs, \(One rhs) -> Lam rhs)
  uniplate (Fix rhs) =
    (One rhs, \(One rhs) -> Fix rhs)
  uniplate (Case lhs alts) = 
    (Two (One lhs) alt_str, 
      \(Two (One lhs) alt_str) -> Case lhs(mkAlts alt_str))
    where
    alt_str = listStr (map altTerm alts)
    mkAlts alt_str = zipWith mkAlt alts (strList alt_str)
    mkAlt (Alt n _) t = Alt n t
