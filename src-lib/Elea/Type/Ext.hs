-- | Functions about types which require type environments and so cannot
-- be in Elea.Type. Notably the 'get' function which returns
-- the type of a term, and the 'check' function which checks a term
-- is correctly typed.
module Elea.Type.Ext
(
  module Elea.Type,
  HasTypeM (..),
  typeOf,
  check,
  checkStep,
)
where

import Elea.Prelude hiding ( get, mapM )
import Elea.Term.Index hiding ( lift )
import Elea.Type hiding ( Var, Var' )
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Prelude as Prelude
import qualified Elea.Term.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Errors.Typing as Err
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail


-- | Return the type of something given a type environment. 
class HasTypeM a where
  getM :: Env.Read m => a -> m Type
  hasM :: Env.Read m => a -> m Bool
  
instance HasTypeM Term where
  getM t = do
    can_type <- hasM t
    if can_type
    then Fold.paraM getM t
    else error ("[typing] Cannot type:\n" ++ show t)
      
  hasM t = do
    offset <- Env.bindingDepth
    return 
      . Env.trackIndices (enum offset)
      $ closed t
    where
    closed :: Term -> Env.TrackIndices Index Bool
    closed (Var x) = do
      offset <- Env.tracked
      return (x < offset)
    closed (Bot _) = return True
    closed (Eql _ _) = return True
    closed (App f _) = closed f
    closed (Fix {}) = return True
    closed (Con {}) = return True
    closed (Lam _ t) = Env.liftTracked (closed t)
    closed (Case _ (Alt _ bs t : _)) = 
      Env.liftTrackedMany (length bs) (closed t)
    

-- | I know I said that terms shouldn't have a 'HasType' instance, but
-- we should only call this on closed terms that we know
-- are well typed.
instance HasType Term where
  get t | has t = (Env.empty . getM) t
  has = Env.empty . hasM
  
instance HasType (Term' (Term, Type)) where 
  has = undefined

  get (Bot' ty) = ty
  get (Eql' _ _) = Type.Base Type.bool
  get (Lam' (Bind _ a) (_, b)) = Type.Fun a b
  get (App' (_, ty) xs) = Type.dropArgs (length xs) ty
  get (Fix' _ b _) = get b
  get (Con' con) = get con
  get (Case' _ (Alt' _ _ (_, ty) : _)) = ty
  
instance HasTypeM (Term' (Term, Type)) where
  hasM = undefined

  getM (Var' x) = do
    is <- Env.isBound x
    if not is
    then error "here"
    else liftM get (Env.boundAt x)
  getM other =
    return (get other)
  

-- | Throws an error if a term is not correctly typed.
check :: (Err.Throws m, Env.Read m, Defs.Read m) => Term -> m ()
-- Call 'typeOf' and ignore the argument
check = liftM (const ()) . typeOf


-- | Wrap this around a term transformation step @Term -> m Term@
-- to add a check that the step preserves the type of the term.
checkStep :: forall m . (Defs.Read m, Env.Read m) => 
  (Term -> m Term) -> Term -> m Term
checkStep step term = do
  result <- step term
  Err.noneM . Err.augmentM (stepErr result) $ do
    t_ty <- typeOf term
    r_ty <- typeOf result
    if t_ty == r_ty
    then return result
    else 
      Err.throw 
        $ "Transformation does not preserve type."
        ++ "\nBefore: [" ++ show t_ty ++ "]"
        ++ "\nAfter: [" ++ show r_ty ++ "]"
  where
  stepErr :: Term -> EitherT String m String
  stepErr result = do
    t_s <- showM term
    t_s' <- showM result
    Err.throw
      $ "In the transformation:"
      ++ "\nFrom: [" ++ t_s ++ "]"
      ++ "\nTo: [" ++ t_s' ++ "]"
      

-- | Returns the type of a given 'Term' while checking it for type errors.
typeOf :: forall m . (Err.Throws m, Env.Read m, Defs.Read m) => Term -> m Type  
typeOf = Fold.paraM getAndCheck
  where
  getAndCheck :: Term' (Term, Type) -> m Type
  getAndCheck t = do
    t_s <- showM (Fold.recover t)
    Err.whileChecking t_s (check' t)
    getM t
    
-- | Takes a term with all subterms already typed, and throws an error
-- if the full term is incorrectly typed.
check' :: forall m . (Err.Throws m, Env.Read m, Defs.Read m) 
  => Term' (Term, Type) -> m ()
  
check' (Var' x)
  | x == Indices.omega = 
    Err.cannotTypeOmega
    
-- Check that a variable has a type in the environment and the arguments
-- match correctly
check' (Var' x) = do
  is_bound <- Env.isBound x
  unless is_bound (Err.unboundIndex (show x))
  
check' (App' (snd -> fun_ty) (map snd -> arg_tys)) = 
  when (arg_tys /= arg_tys') 
    $ Err.invalidArguments (show fun_ty) (show arg_tys)
  where 
  arg_tys' = take (length arg_tys :: Int) (Type.flatten fun_ty)
    
check' (Fix' _ b (t, ty))
  | get b /= ty = do
    t_s <- showM t
    Err.fixBodyBadlyTyped t_s (show (get b)) (show ty)
  
check' (Case' (_, ind_ty) _)
  | not (Type.isInd ind_ty) = 
    Err.patternMatchNonInductive (show ind_ty)
    
check' (Case' (_, Base cse_ind) falts) =
  zipWithM_ checkAlt' [0..] falts
  where
  return_ty = snd (Prelude.get altInner' (head falts))
  
  checkAlt' :: Nat -> Alt' (Term, Type) -> m ()
  checkAlt' alt_n (Alt' con bs (_, alt_ty))
    | con_ind /= cse_ind = Err.incorrectPattern (show con)
    | alt_n /= con_n = Err.patternsOutOfOrder
    | alt_ty /= return_ty = Err.incorrectAltType (show return_ty) (show alt_ty)
    | nlength bs_tys /= length arg_tys = Err.incorrectPattern (show con)
    | or (zipWith (/=) arg_tys bs_tys) = Err.incorrectPattern (show con)
    | otherwise = return ()
    where
    Constructor con_ind con_n = con
    (arg_tys, ind') = id
      . splitAt (length bs) 
      . Type.flatten
      $ get con
    bs_tys = map Type.get bs
    
check' _ = 
  return ()

