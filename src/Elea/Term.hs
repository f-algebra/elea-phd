module Elea.Term
(
  Type, Term (..), Alt (..), Bind (..), FixInfo (..),
  Term' (..), Alt' (..), Bind' (..), FixInfo' (..),
  Matches, ContainsTerms (..), mapTerms,
  projectAlt, embedAlt, 
  projectBind, embedBind,   
  inner, varIndex, alts, argument, 
  inductiveType, binding, constructors,
  altBindings, altInner,
  altBindings', altInner',
  boundLabel, boundType,
  boundLabel', boundType',
  fusedMatches, fusedMatches',
  leftmost, returnType,
  flattenApp, unflattenApp, 
  flattenPi, unflattenPi,
  flattenLam, unflattenLam,
  isInj, isLam, isVar, isPi, isInd, 
  isFix, isAbsurd, isCase,
  fromVar, emptyFixInfo,
  altPattern, argumentCount, 
  fusedMatch, addFusedMatch, addFusedMatches,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

-- * Our functional language

-- | Binding a de-Bruijn index. Might be named, is always typed.
data Bind
  = Bind  { _boundLabel :: !(Maybe String)
          , _boundType :: !Term }
          
instance Eq Bind where
  (==) = (==) `on` _boundType
  
instance Ord Bind where
  compare = compare `on` _boundType
  
-- | I think CoC was invented mostly for this line.
type Type = Term

-- | The de-Bruijn indexed Calculus of Inductive Constructions
-- with general recursion and explicit absurdity.
data Term
  = Var     { _varIndex :: !Index }

  | App     { _inner :: !Term
            , _argument :: !Term }

  | Lam     { _binding :: !Bind 
            , _inner :: !Term }
            
  | Pi      { _binding :: !Bind
            , _inner :: !Term }

  | Fix     { _fixInfo :: !FixInfo
            , _binding :: !Bind 
            , _inner :: !Term }
            
  | Ind     { _binding :: !Bind
            , _constructors :: ![Bind] }

  | Inj     { _constructorIndex :: !Nat 
            , _inductiveType :: !Term }

  | Case    { _inner :: !Term
            , _inductiveType :: !Term
            , _alts :: ![Alt] }

  | Absurd  { _inner :: !Type }
  | Set
  | Type
  deriving ( Eq, Ord )

data Alt
  = Alt   { _altBindings :: ![Bind]
          , _altInner :: !Term }
  deriving ( Eq, Ord )
  
data FixInfo =
  FixInfo { _fusedMatches :: [(Term, Nat)] }

-- The info stored in a 'Fix' has no bearing on its value  
instance Eq FixInfo where
  _ == _ = True
instance Ord FixInfo where
  _ `compare` _ = EQ

emptyFixInfo :: FixInfo
emptyFixInfo = FixInfo mempty
  
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Var' !Index
  | App' a a
  | Lam' !(Bind' a) a
  | Fix' !(FixInfo' a) !(Bind' a) a
  | Pi' !(Bind' a) a
  | Ind' !(Bind' a) ![Bind' a]
  | Inj' !Nat a
  | Case' a a ![Alt' a]
  | Absurd' a
  | Set'
  | Type'
  deriving ( Functor, Foldable, Traversable )
  
data Bind' a
  = Bind' { _boundLabel' :: !(Maybe String)
          , _boundType' :: a }
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altBindings' :: ![Bind' a]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
data FixInfo' a =
  FixInfo'  { _fusedMatches' :: [(a, Nat)] }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Term, ''Alt, ''Bind, ''FixInfo
         , ''Term', ''Alt', ''Bind', ''FixInfo']

-- | A set of equations between terms as mapped keys and constructor terms
-- as mapped values (along with the binding depth when that match was made,
-- this is useful for knowing when not to apply fix-fact fusion).
type Matches = Map Term (Term, Int)

projectAlt :: Alt -> Alt' Term
projectAlt (Alt bs t) = Alt' (map projectBind bs) t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' bs t) = Alt (map embedBind bs) t

projectBind :: Bind -> Bind' Term
projectBind (Bind lbl t) = Bind' lbl t

embedBind :: Bind' Term -> Bind
embedBind (Bind' lbl t) = Bind lbl t

projectFixInfo :: FixInfo -> FixInfo' Term
projectFixInfo (FixInfo ms) = FixInfo' ms

embedFixInfo :: FixInfo' Term -> FixInfo
embedFixInfo (FixInfo' ms) = FixInfo ms

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App t1 t2) = App' t1 t2
  project (Lam b t) = Lam' (projectBind b) t
  project (Fix i b t) = Fix' (projectFixInfo i) (projectBind b) t
  project (Pi b t) = Pi' (projectBind b) t
  project (Ind b cs) = Ind' (projectBind b) (map projectBind cs)
  project (Inj n t) = Inj' n t
  project (Case t ty alts) = Case' t ty (map projectAlt alts)
  project (Absurd ty) = Absurd' ty
  project Set = Set'
  project Type = Type'

instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' t1 t2) = App t1 t2
  embed (Lam' b t) = Lam (embedBind b) t
  embed (Fix' i b t) = Fix (embedFixInfo i) (embedBind b) t
  embed (Pi' b t) = Pi (embedBind b) t
  embed (Ind' b cs) = Ind (embedBind b) (map embedBind cs)
  embed (Inj' n ty) = Inj n ty
  embed (Case' t ty alts) = Case t ty (map embedAlt alts)
  embed (Absurd' ty) = Absurd ty
  embed Set' = Set
  embed Type' = Type
  
class (Substitutable t, Inner t ~ Term) => ContainsTerms t where
  mapTermsM :: Monad m => (Term -> m Term) -> t -> m t
  
mapTerms :: ContainsTerms t => (Term -> Term) -> t -> t
mapTerms f = runIdentity . mapTermsM (return . f)

-- * Some generally helpful functions

isInj :: Term -> Bool
isInj (Inj {}) = True
isInj _ = False

isLam :: Term -> Bool
isLam (Lam {}) = True
isLam _ = False

isVar :: Term -> Bool
isVar (Var {}) = True
isVar _ = False

isPi :: Term -> Bool
isPi (Pi {}) = True
isPi _ = False

isInd :: Term -> Bool
isInd (Ind {}) = True
isInd _ = False

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
    
flattenApp :: Term -> [Term]
flattenApp (App t1 t2) = flattenApp t1 ++ [t2]
flattenApp other = [other]

unflattenApp :: [Term] -> Term
unflattenApp = foldl1 App

flattenLam :: Term -> ([Bind], Term)
flattenLam (Lam b t) = first (b:) (flattenLam t)
flattenLam t = ([], t)

flattenPi :: Term -> ([Bind], Term)
flattenPi (Pi b t) = first (b:) (flattenPi t)
flattenPi t = ([], t)

unflattenLam :: [Bind] -> Term -> Term
unflattenLam = flip (foldr Lam) 

unflattenPi :: [Bind] -> Term -> Term
unflattenPi = flip (foldr Pi) 

-- | Returns the leftmost 'Term' in term application,
-- e.g. @leftmost (App (App a b) c) == a@
leftmost :: Term -> Term
leftmost = head . flattenApp

returnType :: Type -> Type
returnType = snd . flattenPi

-- | If given a function type, returns the number of arguments it takes.
-- If given a function it does the same with its type.
argumentCount :: Type -> Int
argumentCount (Fix _ fix_b _) = 
  argumentCount (get boundType fix_b)
argumentCount pi@(Pi _ _) = 
  length (fst (flattenPi pi))
  
-- | If the provided 'Fix' term has already had a given 
-- pattern match fused into it, this returns which constructor 
-- it was matched to.
fusedMatch :: Term -> Term -> Maybe Nat
fusedMatch match (leftmost -> Fix (FixInfo ms) _ _) =
  lookup match ms
  
addFusedMatch :: (Term, Nat) -> Term -> Term
addFusedMatch (m_t, m_n) (flattenApp -> Fix inf b t : args) = id
  . assert (fusedMatch m_t (Fix inf b t) == Nothing)
  $ unflattenApp (Fix inf' b t : args)
  where
  FixInfo ms = inf
  inf' = FixInfo ((m_t, m_n) : ms)
  b' = modify boundLabel (fmap ("INF@" ++)) b
addFusedMatch _ other = other

addFusedMatches :: [(Term, Nat)] -> Term -> Term
addFusedMatches = flip (foldr addFusedMatch)

-- | Given an inductive type and a constructor index this will return
-- a fully instantiated constructor term.
-- E.g. "altPattern [list] 1 == Cons _1 _0"
altPattern :: Type -> Nat -> Term
altPattern ty@(Ind _ cons) n = id
  . unflattenApp 
  . (Inj n ty :)
  . reverse
  . map (Var . toEnum)
  $ [0..arg_count - 1]
  where
  arg_count = id
    . length
    . fst
    . flattenPi
    . get boundType
    $ cons !! fromEnum n

