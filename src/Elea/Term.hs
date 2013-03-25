{-# LANGUAGE UndecidableInstances #-}
module Elea.Term
(
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Facts (..), IgnoreFactsT (..),
  inner, varIndex, alts, argument, 
  inductiveType, binding,
  altBindings, altInner,
  altBindings', altInner',
  leftmost, flattenApp, unflattenApp, 
  flattenLam, unflattenLam, 
  isInj, isLam, isVar,
  transformTypesM, transformTypes,
  substTypeAt, substType,
  lowerTypes, lowerTypesAt,
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import Elea.Type ( Type, Bind, Env (..), 
                   ReadableEnv (..), bind, bindMany )
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Elea.Monad.Failure as Fail

-- | A de-Bruijn indexed functional language,
-- with term level types ('Type'),
-- general recursion ('Fix'), 
-- inductive data types ('Inj' and 'Case'),
-- and absurdity ('Absurd').
data Term
  = Var     { _varIndex :: !Index }

  | App     { _inner :: !Term
            , _argument :: !Term }

  | Lam     { _binding :: !Bind 
            , _inner :: !Term }

  | Type    !Type

  | Fix     { _binding :: !Bind 
            , _inner :: !Term }

  | Inj     { _constructorIndex :: !Nat 
            , _inductiveType :: !Type }

  | Case    { _inner :: !Term
            , _inductiveType :: !Type
            , _alts :: ![Alt] }

  | Absurd
  deriving ( Eq, Ord )

data Alt
  = Alt   { _altBindings :: ![Bind]
          , _altInner :: !Term }
  deriving ( Eq, Ord )

data Term' a 
  = Var' !Index
  | App' a a
  | Lam' !Bind a
  | Type' !Type
  | Fix' !Bind a
  | Inj' !Nat !Type
  | Case' a !Type ![Alt' a]
  | Absurd'
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altBindings' :: ![Bind]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [''Term, ''Alt, ''Term', ''Alt']
  
type instance Fold.Base Term = Term'

projectAlt :: Alt -> Alt' Term
projectAlt (Alt bs t) = Alt' bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' bs t) = Alt bs t

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App t1 t2) = App' t1 t2
  project Absurd = Absurd'
  project (Type ty) = Type' ty
  project (Lam b t) = Lam' b t
  project (Fix b t) = Fix' b t
  project (Inj n ty) = Inj' n ty
  project (Case t ty alts) = Case' t ty (map projectAlt alts)
  
instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' t1 t2) = App t1 t2
  embed Absurd' = Absurd
  embed (Type' ty) = Type ty
  embed (Lam' b t) = Lam b t
  embed (Fix' b t) = Fix b t
  embed (Inj' n ty) = Inj n ty
  embed (Case' t ty alts) = Case t ty (map embedAlt alts)
  
instance Fold.FoldableM Term where
  type FoldM Term m = (Env m, Facts m)
  
  cataM = fold
    where 
    -- Need to locally scope 'm' and 'a'
    fold :: forall m a . (Env m, Facts m) =>
      (Term' a -> m a) -> Term -> m a
    fold f = join . liftM f . seq . Fold.project
      where
      apply :: Traversable f => f Term -> m (f a)
      apply = sequence . fmap (fold f)
      
      seq :: Term' Term -> m (Term' a)
      seq t@(Lam' b _) =
        bind b (apply t)
      seq t@(Fix' b _) =
        bind b (apply t)
      seq (Case' cse_t ind_ty alts) =
        return (flip Case' ind_ty)
          `ap` fold f cse_t
          `ap` zipWithM seqAlt [0..] alts
        where
        seqAlt :: Nat -> Alt' Term -> m (Alt' a)
        seqAlt n alt@(Alt' bs _) =
            equals cse_t match
          . bindMany bs
          $ apply alt
          where
          match_args = map (Var . toEnum) [(length bs - 1)..0]
          match_con = Inj n ind_ty
          match = unflattenApp (match_con:match_args)
      seq other =
        apply other
        
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

unflattenLam :: [Bind] -> Term -> Term
unflattenLam = flip (foldr Lam) 

-- | Returns the leftmost 'Term' in term application.
-- E.g. "leftmost (App (App a b) c) == a"
leftmost :: Term -> Term
leftmost = head . flattenApp

class Monad m => Facts m where
  equals :: Term -> Term -> m a -> m a
  
-- | Our foldM instance for 'Term' tracks local 'Facts', but many methods
-- need to use fold but don't care about these facts. This wrapper
-- will provide an instance of 'Facts' which ignores any facts it is given.
newtype IgnoreFactsT m a = IgnoreFactsT { ignoreFacts :: m a }
  deriving ( Functor, Monad, Env, ReadableEnv )
  
instance Trans.MonadTrans IgnoreFactsT where
  lift = IgnoreFactsT
  
instance MonadReader r m => MonadReader r (IgnoreFactsT m) where
  local f = Trans.lift . local f . ignoreFacts
  ask = Trans.lift ask
  
instance (Facts m, Monoid w) => Facts (WriterT w m) where
  equals x y = WriterT . equals x y . runWriterT
  
instance Facts m => Facts (MaybeT m) where
  equals x y = MaybeT . equals x y . runMaybeT
  
instance Monad m => Facts (IgnoreFactsT m) where
  equals _ _ = id
  
instance Err.Monad m => Err.Monad (IgnoreFactsT m) where
  throw = Trans.lift . Err.throw
  catch m f = Trans.lift (Err.catch (ignoreFacts m) (ignoreFacts . f))
  
instance Liftable Term where
  liftAt at t = runReader (ignoreFacts (trans t)) at
    where
    trans = transformTypesM liftType <=< Fold.transformM liftVar
    
    liftType :: Type -> IgnoreFactsT (Reader Index) Type
    liftType ty = do
      at <- ask
      return (liftAt at ty)
    
    liftVar :: Term -> IgnoreFactsT (Reader Index) Term
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other
      
instance Monad m => Env (ReaderT Term m) where
  bindAt at _ = local (liftAt at)
  
instance Monad m => Env (ReaderT (Index, Term) m) where
  bindAt at _ = local (liftAt at)

instance Substitutable Term where
  substAt at with term = 
      flip runReader (at, with)
    . ignoreFacts 
    . Fold.transformM substVar
    . lowerTypesAt at
    $ term
    where
    substVar :: Term -> IgnoreFactsT (Reader (Index, Term)) Term
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
      
  freeIndices = flip runReader 0 . ignoreFacts . Fold.foldM free
    where
    free :: Term -> IgnoreFactsT (Reader Index) (Set Index)
    free (Var x) = do
      free_var_limit <- ask
      if x >= free_var_limit
      then return (Set.singleton (x - free_var_limit))
      else return mempty
    free _ = 
      return mempty
    
  failure = 
    App Absurd (Type Type.empty)
      
substTypeAt :: Index -> Type -> Term -> Term
substTypeAt at for =
    flip runReader (at, for)
  . ignoreFacts 
  . transformTypesM substType
  where
  substType :: Type -> IgnoreFactsT (Reader (Index, Type)) Type
  substType t = do
    (at, for) <- ask
    return (substAt at for t)
    
substType :: Type -> Term -> Term
substType = substTypeAt 0

lowerTypesAt :: Index -> Term -> Term
lowerTypesAt = flip substTypeAt Type.empty

lowerTypes :: Term -> Term
lowerTypes = lowerTypesAt 0

instance Monad m => Env (ReaderT () m) where
  bindAt _ _ = id

-- | Applies the given transformation to top-level types within a term.
transformTypesM :: Fold.FoldM Term m =>
  (Type -> m Type) -> Term -> m Term
transformTypesM f = Fold.transformM mapTy
  where
  mapBind = modifyM Type.boundType f 
  
  mapBinds [] = return []
  mapBinds (b:bs) = do
    b' <- mapBind b
    bs' <- bind b' (mapBinds bs)
    return (b':bs')
  
  mapTy (Type ty) = 
    liftM Type (f ty)
  mapTy (Fix b t) = 
    liftM (flip Fix t) (mapBind b)
  mapTy (Lam b t) = 
    liftM (flip Lam t) (mapBind b)
  mapTy (Inj n ind_ty) = 
    liftM (Inj n) (f ind_ty)
  mapTy (Case t ind_ty alts) = do
    ind_ty' <- f ind_ty
    alts' <- mapM mapAltTy alts
    return (Case t ind_ty' alts')
    where
    mapAltTy (Alt bs t) =
      liftM (flip Alt t) (mapBinds bs)
  mapTy other =
    return other
  
transformTypes :: (Type -> Type) -> Term -> Term
transformTypes f = 
    flip runReader () 
  . ignoreFacts 
  . transformTypesM (return . f)
  
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
  
