-- | Elea's type system (System F-omega).
module Elea.Type
(
  Bind (..), boundLabel, boundType, 
  Type (..), FType (..), FBind (..),
  Env (..),  ReadableEnv (..),
  projectBind, embedBind,
  unfoldInd, flattenFun, absurd,
  isInd, isKind,
  bind, bindMany,
  substAt, subst, reduce
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
  = FBind !(Maybe String) !a
  deriving ( Functor, Foldable, Traversable )
  
data FType a
  = FSet
  | FVar !Index
  | FApp !a !a
  | FFun !(FBind a) !a
  | FInd !(FBind a) ![FBind a]
  deriving ( Functor, Foldable, Traversable )
  
type instance Fix.Base Type = FType
  
instance Eq Bind where
  (==) = (==) `on` _boundType
  
instance Ord Bind where
  compare = compare `on` _boundLabel
  
mkLabels [''Bind]

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

instance Fix.FoldableM Type where
  type FoldM Type m = Env m

  foldM = fold
    where
    -- Need to locally scope 'm' and 'a'
    fold :: forall m a . Env m => 
      (FType a -> m a) -> Type -> m a
    fold f = join . liftM f . seq . Fix.project
      where
      apply :: Traversable f => f Type -> m (f a)
      apply = sequence . fmap (fold f)
      
      seq :: FType Type -> m (FType a)
      seq (FFun b t) = do
        b' <- apply b
        t' <- bind (embedBind b) (fold f t)
        return (FFun b' t')
      seq (FInd b cons) = do
        b' <- apply b
        cons' <- bind (embedBind b) (mapM apply cons)
        return (FInd b' cons')
      seq other = 
        apply other

-- | "forall a . a"
absurd :: Type
absurd = Fun (Bind (Just "a") Set) (Var 0)
        
isInd :: Type -> Bool
isInd (Ind {}) = True
isInd _ = False

isKind :: Type -> Bool
isKind Set = True
isKind (Fun (Bind _ ty1) ty2) = 
  isKind ty1 && isKind ty2
isKind _ = False

unfoldInd :: Type -> [Bind]
unfoldInd ty@(Ind _ cons) = 
  map (modify boundType (subst ty)) cons
  
flattenFun :: Type -> ([Bind], Type)
flattenFun (Fun b t) = modify fst (b:) (flattenFun t)
flattenFun t = ([], t)
  
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
  
reduce :: Type -> Type
reduce = Fix.rewrite step
  where
  step (App (Fun _ ret_ty) arg_ty) =
    Just (subst arg_ty ret_ty)
  step _ = Nothing

instance Monad m => Env (ReaderT (Index, Type) m) where
  bindAt at _ = local (liftAt at)
  
substAt :: Index -> Type -> Type -> Type
substAt at with ty = runReader (Fix.transformM substVar ty) (at, with)
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

