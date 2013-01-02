module Elea.Term
(
  Note (..), Index,
  Term (..), InnerTerm (..), Alt (..),
  isInj, leftmost,
  flattenApp, unflattenApp,
  unflattenLam,
  liftAt, lift,
  substAt, substTop,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Lisp ( Lisp )
import qualified Elea.Monad.Error as Error

-- | De-bruijn indices for term variables
newtype Index = Index Int
  deriving ( Eq, Ord )

-- | The annotated, untyped, De-bruijn indexed lambda calculus,
-- with general recursion ('Fix'), 
-- inductive data types ('Inj' and 'Case'),
-- and absurdity ('Absurd').
-- It is parameterised by the type of its annotations.
data Term a
  = Term  { note :: a
          , inner :: !(InnerTerm a) }
 
-- | 'Term's without their annotations.
data InnerTerm a
  = Var !Index
  | App !(Term a) !(Term a)
  | Lam !(Term a)
  | Fix !(Term a)
  | Inj !Int
  | Case !(Term a) ![Alt a]
  | Absurd 
  deriving ( Eq, Ord )
  
-- | The branches of a pattern matching 'Case' expression.
data Alt a 
  = Alt { -- | The number of variables this constructor takes.
          altVars :: !Int
        , altTerm :: !(Term a) }
  deriving ( Eq, Ord )
  
instance Eq (Term a) where
  (==) = (==) `on` inner
  
instance Ord (Term a) where
  compare = compare `on` inner
  
-- | Generalised 'Term' annotations
class Monoid a => Note a where
  -- | Parse term annotations which have been lispified.
  noteFromLisp :: Error.Monad m => Map String Lisp -> m a

  -- | Perform substitution within an annotation on a term,
  -- replacing a given 'Index' with a given 'Term'.
  noteSubstAt :: Index -> Term a -> Term a -> a
  
  -- | Performs index lifing at a given 'Index' 
  -- on an annotation of a term, returning the lifted annotation.
  noteLiftAt :: Index -> Term a -> a
  
  -- | One of the simplification steps from 'Elea.Simplifier'
  -- has been used. The first argument is the original term,
  -- and the second is the term resulting from the simplification.
  -- Which particular simplification was used is inferrable from
  -- the first argument.
  noteSimplified :: Term a -> Term a -> a
  
  
instance Note () where
  noteFromLisp _ = return ()
  noteSubstAt _ _ _ = ()
  noteLiftAt _ _ = ()
  noteSimplified _ _ = ()
  
  
isInj :: Note a => Term a -> Bool
isInj (Term _ (Inj _)) = True
isInj _ = False
      
flattenApp :: Note a => Term a -> [Term a]
flattenApp (inner -> App t1 t2) = t1 : flattenApp t2
flattenApp _ = []
      
unflattenApp :: Note a => [Term a] -> Term a
unflattenApp = foldr1 (\t1 t2 -> Term mempty (App t1 t2))

unflattenLam :: Note a => Int -> Term a -> Term a
unflattenLam 0 t = t
unflattenLam n t 
  | n > 0 = Term mempty (Lam (unflattenLam (n - 1) t))

-- | Returns the leftmost 'Term' in term application.
-- E.g. "leftmost (App (App a b) c) == a"
leftmost :: Note a => Term a -> Term a
leftmost = head . flattenApp
 
-- | Substitute at the outermost De-bruijn index 0
substTop :: Note a => Term a -> Term a -> Term a
substTop = substAt (toEnum 0)
      
-- | Substitute a given De-bruijn 'Index' with a 'Term' within a 'Term'
substAt :: forall a . Note a => Index -> Term a -> Term a -> Term a
substAt at with@(Term _ with_inner) = subst at
  where
  subst :: Index -> Term a -> Term a
  subst at term = 
    Term (noteSubstAt at with term) (substInner (inner term))
    where
    substInner :: InnerTerm a -> InnerTerm a
    substInner Absurd = Absurd
    substInner (Inj n) = Inj n
    substInner (Var var) = 
      case at `compare` var of
        -- Substitution occurs
        EQ -> inner with
        -- Substitution does not occur
        LT -> Var (pred var)
        GT -> Var var
    substInner (t1 `App` t2) = 
      subst at t1 `App` subst at t2
    substInner (Lam rhs) =
      Lam (subst (succ at) rhs)
    substInner (Fix rhs) = 
      Fix (subst (succ at) rhs)
    substInner (Case lhs alts) = 
      Case (subst at lhs) (map substAlt alts)
      where
      substAlt (Alt n t) = Alt n (subst (at `plus` n) t)

-- | Increments every free De-Bruijn index in a 'Term' by one.
-- Equivalent to creating a De-Bruijn index at 0 - see 'liftAt'.
lift :: Note a => Term a -> Term a
lift = liftAt (toEnum 0)

-- | Creates a new De-Bruijn index 
-- by shifting all the later ones up by one (see the 'Var' case).
liftAt :: forall a . Note a => Index -> Term a -> Term a
liftAt at term =
  Term (noteLiftAt at term) (lift (inner term))
  where
  lift :: InnerTerm a -> InnerTerm a
  lift (Var x)
    | at >= x = Var (succ x)
    | otherwise = Var x
  lift (Inj n) = Inj n
  lift Absurd = Absurd
  lift (App t1 t2) = 
    App (liftAt at t1) (liftAt at t2)
  lift (Lam rhs) = 
    Lam (liftAt (succ at) rhs)
  lift (Fix rhs) = 
    Fix (liftAt (succ at) rhs)
  lift (Case lhs alts) =
    Case (liftAt at lhs) (map liftAlt alts)
    where 
    liftAlt (Alt n t) = Alt n (liftAt (at `plus` n) t)

instance Enum Index where
  succ (Index n) = Index (n + 1)
  pred (Index n) | n > 0 = Index (n - 1)
  toEnum n | n >= 0 = Index n
  fromEnum (Index n) = n
  
instance Uniplate (Term a) where
  uniplate (Term note inner) = 
    (str, \str -> Term note (rmk str))
    where
    (str, rmk) = biplate inner
  
instance Biplate (InnerTerm a) (Term a) where
  biplate (Var idx) = 
    (Zero, \Zero -> Var idx)
  biplate (Inj n) =
    (Zero, \Zero -> Inj n)
  biplate Absurd = 
    (Zero, \Zero -> Absurd)
  biplate (App t1 t2) = 
    (Two (One t1) (One t2), \(Two (One t1) (One t2)) -> App t1 t2)
  biplate (Lam rhs) = 
    (One rhs, \(One rhs) -> Lam rhs)
  biplate (Fix rhs) =
    (One rhs, \(One rhs) -> Fix rhs)
  biplate (Case lhs alts) = 
    (Two (One lhs) alt_str, 
      \(Two (One lhs) alt_str) -> Case lhs (mkAlts alt_str))
    where
    alt_str = listStr (map altTerm alts)
    mkAlts alt_str = zipWith mkAlt alts (strList alt_str)
    mkAlt (Alt n _) t = Alt n t

    
