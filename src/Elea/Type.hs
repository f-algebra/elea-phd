-- | Elea's type system (System F-omega).
module Elea.Type
(
  Bind (..), boundLabel, boundType, 
  Type (..), Type' (..), Bind' (..),
  Env (..),  ReadableEnv (..),
  boundLabel', boundType',
  binding, constructors,
  projectBind, embedBind,
  flattenFun, flattenApp,
  unfoldInd, unflattenFun,
  absurd, empty,
  isInd, isKind, isFun,
  bind, bindMany, reduce,
  lowerBind, lowerBindAt,
  removeArg, liftBindAt, liftBind,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import qualified Elea.Monad.Error as Err
import qualified Elea.Foldable as Fold
import qualified Control.Monad.Trans as Trans
import qualified Data.Set as Set

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
  | Ind   { _binding :: !Bind
          , _constructors :: ![Bind] }
  deriving ( Eq, Ord ) 

data Bind' a
  = Bind' { _boundLabel' :: !(Maybe String)
          , _boundType' :: a }
  deriving ( Functor, Foldable, Traversable )

-- | The functor that underlies 'Type', used for 'Elea.Foldable'.
data Type' a
  = Set'
  | Var' !Index
  | App' a a
  | Fun' !(Bind' a) a
  | Ind' !(Bind' a) ![Bind' a]
  deriving ( Functor, Foldable, Traversable )
  
type instance Fold.Base Type = Type'
  
instance Eq Bind where
  (==) = (==) `on` _boundType
  
instance Ord Bind where
  compare = compare `on` _boundType
  
mkLabels [''Type, ''Bind, ''Bind']

embedBind :: Bind' Type -> Bind
embedBind (Bind' lbl t) = Bind lbl t

projectBind :: Bind -> Bind' Type
projectBind (Bind lbl t) = Bind' lbl t

instance Fold.Foldable Type where
  project Set = Set'
  project (Var x) = Var' x
  project (App t1 t2) = App' t1 t2
  project (Fun b t) = Fun' (projectBind b) t
  project (Ind b cons) = Ind' (projectBind b) (map projectBind cons) 
  
instance Fold.Unfoldable Type where
  embed Set' = Set
  embed (Var' x) = Var x
  embed (App' t1 t2) = App t1 t2
  embed (Fun' b t) = Fun (embedBind b) t
  embed (Ind' b cons) = Ind (embedBind b) (map embedBind cons)
  
recoverBind :: Bind' (a, Type) -> Bind 
recoverBind = embedBind . fmap snd

instance Fold.FoldableM Type where
  type FoldM Type m = Env m

  cataM = fold
    where
    -- Need to locally scope 'm' and 'a'
    fold :: forall m a . Env m => 
      (Type' a -> m a) -> Type -> m a
    fold f = join . liftM f . sqn . Fold.project
      where
      apply :: Traversable f => f Type -> m (f a)
      apply = sequence . fmap (fold f)
      
      sqn :: Type' Type -> m (Type' a)
      sqn (Fun' b t) = do
        b' <- apply b
        t' <- bind (embedBind b) (fold f t)
        return (Fun' b' t')
      sqn (Ind' b cons) = do
        b' <- apply b
        cons' <- bind (embedBind b) (mapM apply cons)
        return (Ind' b' cons')
      sqn other = 
        apply other

-- | "forall a . a"
absurd :: Type
absurd = Fun (Bind (Just "a") Set) (Var 0)

empty :: Type 
empty = Ind (Bind (Just "0") Set) []
        
isInd :: Type -> Bool
isInd (Ind {}) = True
isInd _ = False

isFun :: Type -> Bool
isFun (Fun {}) = True
isFun _ = False

-- | Our representation unifies types and kinds.
-- This tests whether a given type is a kind.
isKind :: Type -> Bool
isKind = Fold.cata fkind
  where
  fkind :: Type' Bool -> Bool
  fkind Set' = True
  fkind (Fun' (Bind' _ True) True) = True
  fkind _ = False

unfoldInd :: Type -> [Bind]
unfoldInd ty@(Ind _ cons) = 
  map (modify boundType (subst ty)) cons
  
flattenFun :: Type -> ([Bind], Type)
flattenFun (Fun b t) = first (b:) (flattenFun t)
flattenFun t = ([], t)

unflattenFun :: [Bind] -> Type -> Type
unflattenFun = flip (foldr Fun)
   
flattenApp :: Type -> [Type]
flattenApp (App f x) = flattenApp f ++ [x]
flattenApp other = [other]

reduce :: Type -> Type
reduce (App (Fun _ ret_ty) arg_ty) = 
  arg_ty `subst` ret_ty
reduce other = other

lowerBindAt :: Index -> Bind -> Bind
lowerBindAt at = modify boundType (lowerAt at)

lowerBind :: Bind -> Bind
lowerBind = lowerBindAt 0

liftBindAt :: Index -> Bind -> Bind
liftBindAt at = modify boundType (liftAt at)

liftBind :: Bind -> Bind
liftBind = liftBindAt 0
 
-- | Removes the argument at the given position of a function type.
-- E.g. @removeArg 1 "A->B->C->D" == "A->C->D"
removeArg :: Int -> Type -> Type
removeArg at ty = 
  unflattenFun (start ++ map lowerBind end) (lower result)
  where
  (splitAt at -> (start, mid:end), result) = flattenFun ty

class Monad m => Env m where
  bindAt :: Index -> Bind -> m a -> m a
  
class Env m => ReadableEnv m where
  boundAt :: Index -> m Bind
  
  -- | Returns the greatest index bound within this environment.
  -- If a 'Term' contains an index that is greater than this value
  -- that 'Term' will not be typeable.
  bindingDepth :: m Index
  
bind :: Env m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Env m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

instance Monad m => Env (ReaderT Index m) where
  bindAt at _ = local (liftAt at)

instance Liftable Type where
  liftAt at ty = runReader (Fold.transformM liftVar ty) at
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
  
instance Monad m => Env (ReaderT (Index, Index) m) where
  bindAt at _ = local (liftAt at)
  
instance Monad m => Env (ReaderT [Bind] m) where
  bindAt at b = 
      local 
    $ insertAt (convertEnum at) (liftAt at b) 
    . map (liftAt at)
  
instance Monad m => ReadableEnv (ReaderT [Bind] m) where
  boundAt at = asks (!! fromEnum at)
  bindingDepth = asks (toEnum . pred . length)   

instance Env m => Env (MaybeT m) where
  bindAt at b = MaybeT . bindAt at b . runMaybeT
  
instance ReadableEnv m => ReadableEnv (MaybeT m) where
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance Env m => Env (EitherT e m) where
  bindAt at b = EitherT . bindAt at b . runEitherT
  
instance ReadableEnv m => ReadableEnv (EitherT e m) where
  boundAt = Trans.lift . boundAt
  bindingDepth = Trans.lift bindingDepth
  
instance (Env m, Monoid w) => Env (WriterT w m) where
  bindAt at b = WriterT . bindAt at b . runWriterT
  
instance Substitutable Type where
  substAt at with = 
      flip runReader (at, with) 
    . Fold.transformM substVar
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
      
  freeIndices = flip runReader 0 . Fold.foldM free
    where
    free :: Type -> Reader Index (Set Index)
    free (Var x) = do
      free_var_limit <- ask
      if x >= free_var_limit
      then return (Set.singleton x)
      else return mempty
    free _ = 
      return mempty
      
  failure = empty

