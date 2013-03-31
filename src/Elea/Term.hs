module Elea.Term
(
  Type, Term (..), Alt (..), Bind (..),
  Term' (..), Alt' (..), Bind' (..),
  projectAlt, embedAlt, projectBind, embedBind,   
  inner, varIndex, alts, argument, 
  inductiveType, binding,
  altBindings, altInner,
  altBindings', altInner',
  boundLabel, boundType,
  boundLabel', boundType',
  leftmost, unfoldInd,
  flattenApp, unflattenApp, 
  flattenPi, unflattenPi,
  flattenLam, unflattenLam, 
  isInj, isLam, isVar,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Elea.Monad.Failure as Fail

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

  | Fix     { _binding :: !Bind 
            , _inner :: !Term }
            
  | Ind     { _binding :: !Bind
            , _constructors :: ![Bind] }

  | Inj     { _constructorIndex :: !Nat 
            , _inductiveType :: !Term }

  | Case    { _inner :: !Term
            , _inductiveType :: !Term
            , _alts :: ![Alt] }

  | Absurd
  | Set
  | Type
  deriving ( Eq, Ord )

data Alt
  = Alt   { _altBindings :: ![Bind]
          , _altInner :: !Term }
  deriving ( Eq, Ord )
  
-- * Our typing and equality environment

-- | The write-only environment.
class Monad m => Env m where
  bindAt :: Index -> Bind -> (m a -> m a)
  equal :: Term -> Term -> (m a -> m a)
  
class Env m => ReadableEnv m where
  boundAt :: Index -> m Bind
  bindingDepth :: m Index

  
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Var' !Index
  | App' a a
  | Lam' !(Bind' a) a
  | Fix' !(Bind' a) a
  | Pi' !(Bind' a) a
  | Ind' !(Bind' a) ![Bind' a]
  | Inj' !Nat a
  | Case' a a ![Alt' a]
  | Absurd'
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
  
mkLabels [''Term, ''Alt, ''Bind, ''Term', ''Alt', ''Bind']

projectAlt :: Alt -> Alt' Term
projectAlt (Alt bs t) = Alt' (map projectBind bs) t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' bs t) = Alt (map embedBind bs) t

projectBind :: Bind -> Bind' Term
projectBind (Bind lbl t) = Bind' lbl t

embedBind :: Bind' Term -> Bind
embedBind (Bind' lbl t) = Bind lbl t

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App t1 t2) = App' t1 t2
  project (Lam b t) = Lam' (projectBind b) t
  project (Fix b t) = Fix' (projectBind b) t
  project (Pi b t) = Pi' (projectBind b) t
  project (Ind b cs) = Ind' (projectBind b) (map projectBind cs)
  project (Inj n t) = Inj' n t
  project (Case t ty alts) = Case' t ty (map projectAlt alts)
  project Absurd = Absurd'
  project Set = Set'
  project Type = Type'
  
instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' t1 t2) = App t1 t2
  embed (Lam' b t) = Lam (embedBind b) t
  embed (Fix' b t) = Fix (embedBind b) t
  embed (Pi' b t) = Pi (embedBind b) t
  embed (Ind' b cs) = Ind (embedBind b) (map embedBind cs)
  embed (Inj' n ty) = Inj n ty
  embed (Case' t ty alts) = Case t ty (map embedAlt alts)
  embed Absurd' = Absurd
  embed Set' = Set
  embed Type' = Type
  
        
-- * Some generally helpful functions
        
isInj' :: Term' a -> Bool
isInj' (Inj' {}) = True
isInj' _ = False

isInj :: Term -> Bool
isInj = isInj' . Fold.project

isLam' :: Term' a -> Bool
isLam' (Lam' {}) = True
isLam' _ = False

isLam :: Term -> Bool
isLam = isLam' . Fold.project

isVar :: Term -> Bool
isVar (Var {}) = True
isVar _ = False
    
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

unfoldInd :: Substitutable Term => Term -> [Bind]
unfoldInd ty@(Ind _ cons) = 
  map (modify boundType (subst ty)) cons
  
{-
instance Unifiable Term where
  unifier = (flip runReaderT 0 .) . uni
    where
    uni :: forall m . Fail.Monad m => 
      Term -> Term -> ReaderT Index m (Unifier Term)
    uni (Var x1) (Var x2)
      | x1 == x2 = return mempty
    uni (Var idx) t2 = do
      free_var_limit <- ask
      -- If the variable on the left is not locally scoped
      -- then we can substitute it for something.
      -- We subtract 'free_var_limit' to get the index
      -- outside the bindings of this term.
      if idx >= free_var_limit
      then return (Map.singleton (idx - free_var_limit) t2)
      else Fail.here
    uni Absurd Absurd = return mempty
    uni (Lam b1 t1) (Lam b2 t2) = 
      assert (b1 == b2)
      . local lift 
      $ uni t1 t2
    uni (Fix b1 t1) (Fix b2 t2) = 
      assert (b1 == b2)
      . local lift
      $ uni t1 t2
    uni (App l1 r1) (App l2 r2) = do
      uni1 <- uni l1 l2
      uni2 <- uni r1 r2
      unifierUnion uni1 uni2
    uni (Type ty1) (Type ty2) =
      assert (ty1 == ty2) (return mempty)
    uni (Inj n1 ty1) (Inj n2 ty2) =
      assert (ty1 == ty2) $ do
        Fail.when (n1 /= n2)
        return mempty
    uni (Case match1 _ alts1) (Case match2 _ alts2) = do
      Fail.when (length alts1 /= length alts2)
      match_uni <- uni match1 match2
      alt_unis <- zipWithM (uni `on` get altInner) alts1 alts2
      -- Take the union of the unifiers of the matched terms and the
      -- different pattern branches.
      foldrM unifierUnion match_uni alt_unis
      where
      uniAlt :: Alt -> Alt -> ReaderT Index m (Unifier Term)
      uniAlt (Alt bs1 t1) (Alt bs2 t2) =
        assert (bs1 == bs2)
        . local (liftMany (length bs1))
        $ uni t1 t2
    uni _ _ = Fail.here 
  -}
