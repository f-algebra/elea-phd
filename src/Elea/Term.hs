module Elea.Term
(
  Index, Term (..), Alt (..), Env (..), Liftable (..),
  inner, index, alts, leftmost, 
  altBindings, altTerm,
  flattenApp, unflattenApp, flattenLam,
  transform, bind,
  substAt, subst,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type )
import qualified Elea.Type as Type
import qualified Elea.Monad.Error as Error
import qualified Data.Set as Set
import qualified Data.Label.Maybe as Maybe

-- | De-bruijn indices for term variables
newtype Index = Index Nat
  deriving ( Eq, Ord, Enum )

-- | Bindings for naming and typing a variable.
data Bind 
  = Bind  { _label :: !(Maybe String)
          , _varType :: !Type }
  
-- | De-bruijn indexed System F,
-- with general recursion ('Fix'), 
-- inductive data types ('Inj' and 'Case'),
-- and absurdity ('Absurd').
data Term
  = Var   { _index :: !Index }
  
  | App   { _inner :: !Term
          , _argument :: !Term }
        
  | Fix   { _binding :: !Bind 
          , _inner :: !Term }
        
  | Lam   { _binding :: !Bind 
          , _inner :: !Term }
        
  | Inj   { _injN :: !Int
          , _binding :: !Bind }
        
  | TyApp { _inner :: !Term
          , _typeArgument :: !Type }
          
  | TyLam { _typeBinding :: !Type.Bind
          , _inner :: !Term }
          
  | Case  { _inner :: !Term
          , _alts :: ![Alt] }
          
  | Absurd 
  deriving ( Eq, Ord )
  
-- | The branches of a pattern matching 'Case' expression.
data Alt
  = Alt { _altBindings :: ![Bind]
        , _altTerm :: !Term }
  deriving ( Eq, Ord )
  
instance Eq Bind where
  (==) = (==) `on` _varType
  
instance Ord Bind where
  compare = compare `on` _varType
  
mkLabels [''Bind, ''Term, ''Alt]

flattenApp :: Term -> [Term]
flattenApp (App t1 t2) = flattenApp t1 ++ [t2]
flattenApp other = [other]

unflattenApp :: [Term] -> Term
unflattenApp = foldl1 App

flattenLam :: Term -> ([Bind], Term)
flattenLam (Lam b t) = modify fst (b:) (flattenLam t)
flattenLam t = ([], t)

-- | Returns the leftmost 'Term' in term application.
-- E.g. "leftmost (App (App a b) c) == a"
leftmost :: Term -> Term
leftmost = head . flattenApp

-- | Applies a 'Term' transformation within a 'Term', supplying a term
-- and type variable binding environment at each point.
transform :: (MonadReader r m, Env r) =>
  (Term -> m Term) -> Term -> m Term
transform f = mapT
  where
  mapT t = f =<< mp t
  
  mp (App t1 t2) = do
    t1' <- mapT t1
    t2' <- mapT t2
    return (App t1' t2')
  mp (TyApp t ty) = 
    liftM (flip TyApp ty) $ mapT t
  mp (Fix b t) = 
    liftM (Fix b) $ local (bind b) $ mapT t
  mp (Lam b t) =
    liftM (Lam b) $ local (bind b) $ mapT t
  mp (TyLam tyb t) = 
    liftM (TyLam tyb) $ local (Type.bind tyb) $ mapT t
  mp (Case t alts) = do
    t' <- mapT t
    alts' <- mapM mapAlt alts
    return (Case t' alts')
    where
    mapAlt (Alt bs t) = 
      liftM (Alt bs) $ local (bindMany bs) $ mapT t
  mp other = 
    return other
    
transformTypes :: (MonadReader r m, Env r) =>
  (Type -> m Type) -> Term -> m Term
transformTypes f = transform mapTy
  where
  mapBind = modifyM varType f 
  
  mapTy (Fix b t) = 
    liftM (flip Fix t) $ mapBind b
  mapTy (Lam b t) = 
    liftM (flip Lam t) $ mapBind b
  mapTy (TyApp t ty) = 
    liftM (TyApp t) $ f ty
  mapTy (Case t alts) =
    liftM (Case t) $ mapM mapAlt alts
    where
    mapAlt = modifyM altBindings (mapM mapBind)
  mapTy other =
    return other
    
class Liftable a where
  liftAt :: Index -> a -> a
  
instance Liftable Index where
  liftAt at x
    | at <= x = succ x
    | otherwise = x 
    
instance Liftable Term where
  liftAt at t = runReader (transform liftVar t) at
    where
    liftVar :: MonadReader Index m => Term -> m Term
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other    
      
instance Type.Liftable Term where
  liftAt at term = runReader (transformTypes liftTypes term) at
    where
    liftTypes :: MonadReader Type.Index m => Type -> m Type
    liftTypes = ap (asks Type.liftAt) . return
      
-- | An environment which stores term 'Bind'ings
class Type.Env a => Env a where
  bindAt :: Index -> Bind -> a -> a
  
bind :: Env a => Bind -> a -> a
bind = bindAt (toEnum 0)
  
bindMany :: Env a => [Bind] -> a -> a
bindMany = concatEndos . map bind
      
instance Type.Env Index where
  -- Term indices are obviously unaffected by binding type indices
  bindAt at _ = id
  
instance Env Type.Index where
  -- Type indices are obviously unaffected by binding term indices
  bindAt at _ = id

instance Env Index where
  bindAt at _ = liftAt at
  
instance Type.Env Term where
  bindAt at _ = Type.liftAt at
  
instance Env Term where
  bindAt at _ = liftAt at
  
instance Env Type where
  -- Types are obviously unaffected by binding term indices
  bindAt at _ = id
  
instance (Env a, Env b) => Env (a, b) where
  bindAt at b (x, y) = (bindAt at b x, bindAt at b y)

substAt :: Index -> Term -> Term -> Term
substAt at with term = runReader (transform substVar term) (at, with)
  where
  substVar :: MonadReader (Index, Term) m => Term -> m Term
  substVar (Var var) = do
    (at, with) <- ask
    return $ case at `compare` var of
      -- Substitution occurs
      EQ -> with
      -- Substitution does not occur
      LT -> Var (pred var)
      GT -> Var var
  substVar other =
    return other
    
-- | Substitute at the outermost De-bruijn index 0
subst :: Term -> Term -> Term
subst = substAt (toEnum 0)

substTypeAt :: Type.Index -> Type -> Term -> Term
substTypeAt at for term = 
  runReader (transformTypes substType term) (at, for)
  where
  substType :: MonadReader (Type.Index, Type) m => Type -> m Type
  substType t = do
    (at, for) <- ask
    return (Type.substAt at for t)
    
substType :: Type -> Term -> Term
substType = substTypeAt (toEnum 0)

  


