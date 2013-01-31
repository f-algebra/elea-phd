-- | Elea's type system
module Elea.Type
(
  Type (..), Bind (..), Env (..), Liftable (..), Index,
  liftAll, bind, transform, label, subst, substAt,
)
where

import Prelude ()
import Elea.Prelude

-- | De-bruijn indices for type variables
newtype Index = Index Nat
  deriving ( Eq, Ord, Enum )

-- | Bindings for type variables
data Bind
  = Bind  { _label :: !(Maybe String) }
  deriving ( Eq, Ord )
  
-- | De-bruijn indexed System F, with inductive data types.
data Type  
  = Var !Index
  | Fun !Type !Type
  | All !Bind !Type
  | Ind !Bind ![[Type]]
  deriving ( Eq, Ord )
  
mkLabels [''Bind]

class Liftable a where
  liftAt :: Index -> a -> a

liftAll :: Liftable a => a -> a
liftAll = liftAt (toEnum 0)  

instance Liftable Index where
  liftAt at x
    | at <= x = succ x
    | otherwise = x 
  
instance Liftable Type where
  liftAt at ty = runReader (transform liftVar ty) at
    where
    liftVar :: MonadReader Index m => Type -> m Type
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other
      
class Env a where
  bindAt :: Index -> Bind -> a -> a
      
instance Env Type where
  bindAt at _ = liftAt at
  
instance Env Index where
  bindAt at _ = liftAt at
  
instance (Env a, Env b) => Env (a, b) where
  bindAt at b (x, y) = (bindAt at b x, bindAt at b y)
    
bind :: Env a => Bind -> a -> a
bind = bindAt (toEnum 0)
  
transform :: (MonadReader r m, Env r) => 
  (Type -> m Type) -> Type -> m Type
transform f = mapT
  where
  mapT t = f =<< mp t
  
  mp (Fun t1 t2) = do
    t1' <- mapT t1
    t2' <- mapT t2
    return (Fun t1' t2')
  mp (All b t) = 
    liftM (All b) $ local (bind b) $ mapT t
  mp (Ind b t) = 
    liftM (Ind b) $ local (bind b) $ mapM (mapM mapT) t
  mp other =
    return other
    
substAt :: Index -> Type -> Type -> Type
substAt at with ty = runReader (transform substVar ty) (at, with)
  where
  substVar :: MonadReader (Index, Type) m => Type -> m Type
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
subst = substAt (toEnum 0)

instance Show Index where
  show (Index n) = "_" ++ show n
  
