-- | Functions about types which require type environments and so cannot
-- be in Elea.Type. Notably the 'get' function which returns
-- the type of a term, and the 'check' function which checks a term
-- is correctly typed.
module Elea.Types
(
  module Elea.Type,
  HasTypeM (..),
  typeOf,
  isClosed,
  check,
)
where

import Elea.Prelude hiding ( get, mapM )
import Elea.Index hiding ( lift )
import Elea.Type hiding ( Var, Var' )
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Prelude as Prelude
import qualified Elea.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Errors.Typing as Err
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail


-- | Return the type of something given a type environment. 
class HasTypeM a where
  getM :: Env.Bindings m => a -> m Type
  
instance HasTypeM Term where
  getM = Fold.paraM getM

-- | Whether we can use the 'get' function from 'HasType' to type a term.
isClosed :: Term -> Bool
isClosed = Env.trackOffset . clsd
  where
  clsd :: Term -> Env.TrackOffset Bool
  clsd (Var x _) = Env.lowerableByOffset x
  clsd (Def {}) = return True
  clsd (Con {}) = return True
  clsd (Lam _ t) = Env.liftTracked (clsd t)
  clsd (Case _ (Alt _ bs t : _)) = 
    Env.liftTrackedMany (length bs) (clsd t)
    

-- | I know I said that terms shouldn't have a 'HasType' instance, but
-- we should only call this on closed terms that we know
-- are well typed.
instance HasType Term where
  get = Env.empty . getM
  
instance HasType (Term' (Term, Type)) where 
  get (Unr' ty) = ty
  get (Lam' (Bind _ a) (_, b)) = Type.Fun a b
  get (Def' iname xs) = 
    dropArgs (length xs) (get iname)
  get (Con' con xs) = 
    dropArgs (length xs) (get con)
  get (Case' _ (Alt' _ _ (_, ty) : _)) = ty
  
instance HasTypeM (Term' (Term, Type)) where
  getM (Var' x xs) =
    liftM (Type.dropArgs (length xs) . Type.get) (Env.boundAt x)
  getM other =
    return (get other)
  

-- | Throws an error if a term is not correctly typed.
check :: (Err.Throws m, Env.Bindings m) => Term -> m ()
-- Call 'typeOf' and ignore the argument
check = liftM (const ()) . typeOf

-- | Returns the type of a given 'Term' while checking it for type errors.
typeOf :: forall m . (Err.Throws m, Env.Bindings m) => Term -> m Type  
typeOf = Fold.paraM getAndCheck
  where
  getAndCheck :: Term' (Term, Type) -> m Type
  getAndCheck t = do
    t_s <- showM (Fold.recover t)
    Err.whileChecking t_s (check' t)
    getM t
    
    
-- | Throw an error if the arguments of a given type do not match.
-- > checkArgs (A -> B -> C) [A, B] = return ()
-- > checkArgs (A -> B -> C) [B, A] = Err.throw ...
checkArgs :: Err.Throws m => Type -> [Type] -> m ()
checkArgs fun_ty arg_tys = 
  when (arg_tys /= arg_tys') 
    $ Err.invalidArguments (show fun_ty) (show arg_tys)
  where 
  arg_tys' = take (length arg_tys :: Int) (Type.flatten fun_ty)
  
    
-- | Takes a term with all subterms already typed, and throws an error
-- if the full term is incorrectly typed.
check' :: forall m . (Err.Throws m, Env.Bindings m) 
  => Term' (Term, Type) -> m ()
  
check' (Var' x xs)
  | x == Indices.omega = 
    Err.cannotTypeOmega
    
-- Check that a variable has a type in the environment and the arguments
-- match correctly
check' (Var' x xs) = do
  is_bound <- Env.isBound x
  if not is_bound
  then Err.unboundIndex (show x)
  else do
    x_b <- Env.boundAt x
    checkArgs (Type.get x_b) (map snd xs)
    
check' (Def' name xs) = 
  checkArgs (get name) (map snd xs)
check' (Con' con xs) = 
  checkArgs (get con) (map snd xs)
  
check' (Case' (_, ind_ty) _)
  | not (Type.isInd ind_ty) = 
    Err.patternMatchNonInductive (show ind_ty)
    
check' (Case' (_, Base cse_ind) falts) =
  zipWithM_ checkAlt' [0..] falts
  where
  return_ty = snd (altTerm' (head falts))
  
  checkAlt' :: Nat -> Alt' (Term, Type) -> m ()
  checkAlt' alt_n (Alt' con bs (_, alt_ty))
    | con_ind /= cse_ind = Err.incorrectPattern (show con)
    | alt_n /= con_n = Err.patternsOutOfOrder
    | alt_ty /= return_ty = Err.incorrectAltType (show return_ty) (show alt_ty)
    | nlength bs_tys /= length arg_tys = Err.incorrectPattern (show con)
    | or (zipWith (/=) arg_tys bs_tys) = Err.incorrectPattern (show con)
    where
    (arg_tys, ind') = id
      . splitAt (length bs) 
      . Type.flatten
      $ get con
    bs_tys = map Type.get bs
    con_ind = fmap constructorOf con
    con_n = constructorIndex (instObj con)
    
check' _ = 
  return ()


