module Elea.Env 
(
  Writable (..), Readable (..),
  bind, bindMany,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

class Monad m => Writable m where
  bindAt :: Index -> Bind -> m a -> m a
  equals :: Term -> Term -> m a -> m a
  
bind :: Writable m => Bind -> m a -> m a
bind = bindAt 0

bindMany :: Writable m => [Bind] -> m a -> m a
bindMany = concatEndos . map bind

class Writable m => Readable m where
  boundAt :: Index -> m Bind
  bindingDepth :: m Index
  
instance Fold.FoldableM Term where
  type FoldM Term m = Writable m
  
  distM (Lam' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Lam' (Bind' l ty) t)
  distM (Fix' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Fix' (Bind' l ty) t)
  distM (Pi' (Bind' l (mty, _ty)) (mt, _)) = do
    ty <- mty
    t <- bind (Bind l _ty) mt
    return (Pi' (Bind' l ty) t)
  distM (Ind' (Bind' l (mty, _ty)) mcs) = do
    ty <- mty
    cs <- bind (Bind l _ty) (mapM (sequence . map fst) mcs)
    return (Ind' (Bind' l ty) cs)
  distM (Case' (mt, _t) (mty, _ty) malts) = do
    alts <- zipWithM distAlt [0..] malts
    t <- mt
    ty <- mty
    return (Case' t ty alts)
    where
    distAlt n (Alt' mbs (mt, _t)) = do
      bs <- mapM (sequence . map fst) mbs
      t <- bindMany _bs
         $ (_t `equals` match_t) mt
      return (Alt' bs t)
      where
      _bs = map (embedBind . map snd) mbs
      match_args = reverse 
        . map (Var . toEnum)
        $ [0..length mbs - 1]
      match_con = Inj n _ty
      match_t = unflattenApp (match_con : match_args)
  distM other = 
    sequence (map fst other)
    
instance Liftable Term where
  liftAt at = flip runReader at . Fold.transformM liftVar
    where
    liftVar :: Term -> Reader Index Term
    liftVar (Var idx) = do
      at <- ask
      return $ Var (liftAt at idx)
    liftVar other = 
      return other
      
instance Liftable Bind where
  liftAt at (Bind l t) = Bind l (liftAt at t)
      
instance Substitutable Term where
  substAt at with term = 
      flip runReader (at, with)
    . Fold.transformM substVar
    $ term
    where
    substVar :: Term -> Reader (Index, Term) Term
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
      
  freeIndices = flip runReader 0 . Fold.foldM free
    where
    free :: Term -> Reader Index (Set Index)
    free (Var x) = do
      free_var_limit <- ask
      if x >= free_var_limit
      then return (Set.singleton (x - free_var_limit))
      else return mempty
    free _ = 
      return mempty
      
  failure = Absurd
  
instance Monad m => Writable (ReaderT Index m) where
  bindAt at _ = local (liftAt at)
  equals _ _ = id
  
instance Monad m => Writable (ReaderT (Index, Term) m) where
  bindAt at _ = local (liftAt at)
  equals _ _ = id
  
instance (Monoid w, Writable m) => Writable (WriterT w m) where
  bindAt at b = WriterT . bindAt at b . runWriterT
  equals t1 t2 = WriterT . equals t1 t2 . runWriterT
  
instance Monad m => Writable (ReaderT [Bind] m) where
  bindAt at b = 
      local 
    $ insertAt (convertEnum at) (liftAt at b) 
    . map (liftAt at)
  equals _ _ = id
  
instance Monad m => Readable (ReaderT [Bind] m) where
  boundAt at = asks (!! fromEnum at)
  bindingDepth = asks (toEnum . pred . length)   
  
