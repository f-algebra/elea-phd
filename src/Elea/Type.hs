-- | Elea's type system (System F-omega).
module Elea.Type
(
  Bind (..), boundLabel, boundType, 
  Type (..), FType (..), FBind (..),
  Env (..),  ReadableEnv (..),
  fBoundLabel, fBoundType,
  projectBind, embedBind,
  flattenFun, flattenApp,
  unfoldInd, absurd,
  isInd, isKind, isFun,
  bind, bindMany,
  substAt, subst,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import qualified Elea.Foldable as Fix

-- | Binding a de-Bruijn index. Might be named, is always typed.
data Bind
  = Bind  { _boundLabel :: !(Maybe String)
          , _boundType :: !Type }

-- | System F-omega with inductive data-types.
data Type
  = Set
  | Var !Index
  | App !Type !Type
  | Fun !Bind !Type
  | Ind !Bind ![Bind]
  deriving ( Eq, Ord ) 

data FBind a
  = FBind { _fBoundLabel :: !(Maybe String)
          , _fBoundType :: a }
  deriving ( Functor, Foldable, Traversable )

-- | The functor that underlies 'Type', used for 'Elea.Foldable'.
data FType a
  = FSet
  | FVar !Index
  | FApp a a
  | FFun !(FBind a) a
  | FInd !(FBind a) ![FBind a]
  deriving ( Functor, Foldable, Traversable )
  
type instance Fix.Base Type = FType
  
instance Eq Bind where
  (==) = (==) `on` _boundType
  
instance Ord Bind where
  compare = compare `on` _boundLabel
  
mkLabels [''Bind, ''FBind]

embedBind :: FBind Type -> Bind
embedBind (FBind lbl t) = Bind lbl t

projectBind :: Bind -> FBind Type
projectBind (Bind lbl t) = FBind lbl t

instance Fix.Foldable Type where
  project Set = FSet
  project (Var x) = FVar x
  project (App t1 t2) = FApp t1 t2
  project (Fun b t) = FFun (projectBind b) t
  project (Ind b cons) = FInd (projectBind b) (map projectBind cons) 
  
instance Fix.Unfoldable Type where
  embed FSet = Set
  embed (FVar x) = Var x
  embed (FApp t1 t2) = App t1 t2
  embed (FFun b t) = Fun (embedBind b) t
  embed (FInd b cons) = Ind (embedBind b) (map embedBind cons)
  
recoverBind :: FBind (a, Type) -> Bind 
recoverBind = embedBind . fmap snd

instance Fix.FoldableM Type where
  type FoldM Type m = Env m

  cataM = fold
    where
    -- Need to locally scope 'm' and 'a'
    fold :: forall m a . Env m => 
      (FType a -> m a) -> Type -> m a
    fold f = join . liftM f . sqn . Fix.project
      where
      apply :: Traversable f => f Type -> m (f a)
      apply = sequence . fmap (fold f)
      
      sqn :: FType Type -> m (FType a)
      sqn (FFun b t) = do
        b' <- apply b
        t' <- bind (embedBind b) (fold f t)
        return (FFun b' t')
      sqn (FInd b cons) = do
        b' <- apply b
        cons' <- bind (embedBind b) (mapM apply cons)
        return (FInd b' cons')
      sqn other = 
        apply other

-- | "forall a . a"
absurd :: Type
absurd = Fun (Bind (Just "a") Set) (Var 0)
        
isInd :: Type -> Bool
isInd (Ind {}) = True
isInd _ = False

isFun :: Type -> Bool
isFun (Fun {}) = True
isFun _ = False

-- | Our representation unifies types and kinds.
-- This tests whether a given type is a kind.
isKind :: Type -> Bool
isKind = Fix.cata fkind
  where
  fkind :: FType Bool -> Bool
  fkind FSet = True
  fkind (FFun (FBind _ True) True) = True
  fkind _ = False

unfoldInd :: Type -> [Bind]
unfoldInd ty@(Ind _ cons) = 
  map (modify boundType (subst ty)) cons
  
flattenFun :: Type -> ([Bind], Type)
flattenFun (Fun b t) = first (b:) (flattenFun t)
flattenFun t = ([], t)
   
flattenApp :: Type -> [Type]
flattenApp (App f x) = flattenApp f ++ [x]
flattenApp other = [other]
  
class Monad m => Env m where
  bindAt :: Index -> Bind -> m a -> m a
  
class Env m => ReadableEnv m where
  boundAt :: Index -> m Bind
  
bind :: Env m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Env m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

instance Monad m => Env (ReaderT Index m) where
  bindAt at _ = local (liftAt at)

instance Liftable Type where
  liftAt at ty = runReader (Fix.transformM liftVar ty) at
    where
    liftVar :: Type -> Reader Index Type
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other
      
instance Liftable Bind where
  liftAt at (Bind lbl ty) = Bind lbl (liftAt at ty)

instance Monad m => Env (ReaderT (Index, Type) m) where
  bindAt at _ = local (liftAt at)
  
substAt :: Index -> Type -> Type -> Type
substAt at with = 
    flip runReader (at, with) 
  . Fix.transformM substVar
  where
  substVar :: Type -> Reader (Index, Type) Type
  substVar (Var idx) = do
    (at, with) <- ask
    return $ case at `compare` idx of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred idx)
      GT -> Var idx
  substVar other = 
    return other

subst :: Type -> Type -> Type
subst = substAt 0

