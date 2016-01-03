-- | Functions about types which require type environments and so cannot
-- be in Elea.Type. Notably the 'get' function which returns
-- the type of a term, and the 'check' function which checks a term
-- is correctly typed.
module Elea.Type.Ext
(
  module Elea.Type,
  HasTypeM (..),
  assertEqM,
)
where

import Elea.Prelude hiding ( get )
import Elea.Term.Index hiding ( lift )
import Elea.Type hiding ( Var, Var' )
import Elea.Term
import Elea.Show ()
import Elea.Show.Class ( ShowM (..) )
import qualified Elea.Term as Term
import qualified Elea.Prelude as Prelude
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Errors.Typing as Err
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set


-- | Return the type of something given a type environment. 
class Show a => HasTypeM a where
  assignM :: (Fail.Can m, Env.Read m) => a -> m (Maybe Type)
  
  getM :: Env.Read m => a -> m Type
  getM x = do
    as_x <- runMaybeT (assignM x)
    case as_x of
      Just (Just ty) -> return ty
      _ -> error ("[Type.getM failed] " ++ show x)
  
  hasM :: Env.Read m => a -> m Bool
  hasM x = do
    as_x <- runMaybeT (assignM x)
    case as_x of
      Just (Just _) -> return True
      Just Nothing -> return False
      Nothing ->
        error ("[Type.has failed] " ++ show x)
      
  validM :: Env.Read m => a -> m Bool
  validM = liftM isJust . runMaybeT . assignM
  
  
instance HasTypeM Term where
  assignM = Fold.cataM check
    where
    check :: forall m . (Fail.Can m, Env.Read m) 
      => Term' (Maybe Type) -> m (Maybe Type)
    check (Var' x _) = do
      has <- Env.isBound x
      if not has
      then return Nothing
      else liftM (Just . Prelude.get Type.bindType) (Env.boundAt x)
    check (Bot' ty) = return (Just ty)
    check (Leq' (Just xt) (Just yt)) = do
      Fail.when (xt /= yt)
      return (Just (Base prop))
    check (Leq' _ _) = 
      return (Just (Base prop))
    check (Seq' _ (Just ty)) = do
      return (Just ty)
    check (Seq' _ Nothing) = 
      return Nothing
    check (App' Nothing _) =
      return Nothing
    check (App' (Just fty) xs) = do
      Fail.unless (length arg_tys >= length xs)
      Fail.unless (Prelude.and (zipWith checkArg xs arg_tys))
      return 
        . Just 
        $ dropArgs (nlength xs) fty
      where
      (arg_tys, res_ty) = split fty
      
      checkArg :: Maybe Type -> Type -> Bool
      checkArg Nothing _ = True
      checkArg (Just ty) arg_ty = 
        ty == arg_ty
        
    check (Fix' fix_i (Bind _ fix_ty) mby_ty) = do
   --   valid_cts <- (allM validM . Set.toList . Prelude.get fixDomain) fix_i
   --   Fail.unless valid_cts
      Fail.unless (isNothing mby_ty || fix_ty == fromJust mby_ty)
      return (Just fix_ty)
    check (Con' con) = 
      return (Just (get con))
    check (Lam' _ Nothing) = 
      return Nothing
    check (Lam' (Bind _ arg_ty) (Just res_ty))  
      | res_ty == Type.Base Type.prop = 
        return (Just (Type.Base Type.prop))
      | otherwise =
        return (Just (Fun arg_ty res_ty))
    check (Case' cse_ty alts) = do
      -- Check the pattern match term is inductively typed
      Fail.unless (isNothing cse_ty || (isInd . fromJust) cse_ty)
      
      mby_alt_tys <- mapM checkAlt alts
      let alt_tys = nubOrd (catMaybes mby_alt_tys)
      case alt_tys of
        [] -> return Nothing
        [ty] -> return (Just ty)
        _ -> Fail.here
          -- ^ Branches disagree on type
      where
      checkAlt :: Alt' (Maybe Type) -> m (Maybe Type)
      checkAlt (Alt' tcon _ ty) = do
        Fail.unless (isNothing cse_ty 
          || cse_ind == Prelude.get constructorOf (Tag.untag tcon))
        return ty
        where
        Just (Base cse_ind) = cse_ty
        
instance HasTypeM Match where
  assignM m = do
    mby_mty <- assignM (matchedTerm m)
    mby_pty <- assignM (matchedTo m)
    case (mby_mty, mby_pty) of
      (Just ty, _) -> return (Just ty)
      (_, Just ty) -> return (Just ty)
      _ -> return Nothing
    

-- | I know I said that terms shouldn't have a 'HasType' instance, but
-- we should y call this on closed terms that we know
-- are well typed.
instance HasType Term where
  assign = Env.emptyT . assignM


assertEqM :: (Env.Read m, HasTypeM a, ShowM m a) 
  => String -> a -> a -> m ()
assertEqM msg x y = do
  as_x <- runMaybeT (assignM x)
  as_y <- runMaybeT (assignM y)
  if valid as_x as_y
  then return ()
  else do
    x_s <- showM x
    y_s <- showM y
    let x_ty = showTyping as_x
        y_ty = showTyping as_y
        
    error 
      $  "\n\n[type error] " ++ msg 
      ++ "\n[original term] " ++ x_s ++ " : " ++ x_ty
      ++ "\n[new term] " ++ y_s ++ " : " ++ show y_ty
  where
  valid (Just (Just t1)) (Just (Just t2)) = 
    t1 == t2
  valid _ _ = False
  
  showTyping Nothing = "[invalid type]"
  showTyping (Just Nothing) = "[type unknown]"
  showTyping (Just (Just ty)) = show ty

