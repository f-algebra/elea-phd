module Elea.Term
(
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Type, Bind (..),
  ContainsTerms (..),
  mapTerms,
  app,
  projectAlt, embedAlt,
  altBindings, altInner,
  altBindings', altInner',
  flattenApp, leftmost, arguments,
  flattenLam, unflattenLam,
  isCon, isLam, isVar,
  isFix, isAbsurd, isCase,
  fromVar, altPattern, 
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Type ( Type, Bind (..) )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

-- | The simply typed lambda calculus
data Term
  = Var     { varIndex :: !Index }

  | App     !Term ![Term]

  | Lam     { binding :: !Bind 
            , inner :: !Term }

  | Fix     { binding :: !Bind 
            , inner :: !Term }

  | Con     { inductiveType :: !Type.Ind
            , constructorIndex :: !Nat }

  | Case    { inductiveType :: !Type.Ind
            , inner :: !Term
            , alts :: ![Alt] }

  | Absurd  { resultType :: !Type }
  deriving ( Eq, Ord )

data Alt
  = Alt   { _altBindings :: ![Bind]
          , _altInner :: !Term }
  deriving ( Eq, Ord )
  
  
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Var' !Index
  | App' a [a]
  | Lam' !Bind a
  | Fix' !Bind a
  | Con' !Type.Ind !Nat
  | Case' !Type.Ind a ![Alt' a]
  | Absurd' !Type
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altBindings' :: ![Bind]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Alt, ''Alt' ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt bs t) = Alt' bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' bs t) = Alt bs t

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App f xs) = App' f xs
  project (Lam b t) = Lam' b t
  project (Fix b t) = Fix' b t
  project (Con ind n) = Con' ind n
  project (Case ind t alts) = Case' ind t (map projectAlt alts)
  project (Absurd ty) = Absurd' ty

instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' f xs) = App f xs
  embed (Lam' b t) = Lam b t
  embed (Fix' b t) = Fix b t
  embed (Con' ind n) = Con ind n
  embed (Case' ind t alts) = Case ind t (map embedAlt alts)
  embed (Absurd' ty) = Absurd ty
  
class (Substitutable t, Inner t ~ Term) => ContainsTerms t where
  mapTermsM :: Monad m => (Term -> m Term) -> t -> m t
  
mapTerms :: ContainsTerms t => (Term -> Term) -> t -> t
mapTerms f = runIdentity . mapTermsM (return . f)

-- * Some generally helpful functions

app :: Term -> [Term] -> Term
app f [] = f
app (App f xs) ys = App f (xs ++ ys)
app f xs = App f xs

isCon :: Term -> Bool
isCon (Con {}) = True
isCon _ = False

isLam :: Term -> Bool
isLam (Lam {}) = True
isLam _ = False

isVar :: Term -> Bool
isVar (Var {}) = True
isVar _ = False

isFix :: Term -> Bool
isFix (Fix {}) = True
isFix _ = False

isCase :: Term -> Bool
isCase (Case {}) = True
isCase _ = False

isAbsurd :: Term -> Bool
isAbsurd (Absurd {}) = True
isAbsurd _ = False

fromVar :: Term -> Index
fromVar (Var x) = x

flattenLam :: Term -> ([Bind], Term)
flattenLam (Lam b t) = first (b:) (flattenLam t)
flattenLam t = ([], t)

unflattenLam :: [Bind] -> Term -> Term
unflattenLam = flip (foldr Lam)

flattenApp :: Term -> [Term]
flattenApp (App f xs) = f:xs
flattenApp t = [t]

leftmost :: Term -> Term
leftmost = head . flattenApp

arguments :: Term -> [Term]
arguments = tail . flattenApp
  
-- | Given an inductive type and a constructor index this will return
-- a fully instantiated constructor term.
-- E.g. "altPattern [list] 1 == Cons _1 _0"
altPattern :: Type.Ind -> Nat -> Term
altPattern ty@(Type.Ind _ cons) n = id
  . app (Con ty n)
  . reverse
  . map (Var . enum)
  $ [0..arg_count - 1]
  where
  arg_count = id
    . length
    . snd
    $ cons !! fromEnum n

