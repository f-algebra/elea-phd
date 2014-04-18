-- | Functions about types which require type environments and so cannot
-- be in Elea.Type. Notably the 'get' function which returns
-- the type of a term, and the 'check' function which checks a term
-- is correctly typed.
module Elea.Types
(
  module Elea.Type,
  typeOf,
  isClosed,
  getClosed,
  get, 
  check,
)
where

import Elea.Prelude hiding ( get, mapM )
import Elea.Index hiding ( lift )
import Elea.Type
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Prelude as Prelude
import qualified Elea.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Errors.Typing as Err
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail


-- | Return the type of a term. Assumes the term is well-typed.
get :: Env.Bindings m => Term -> m Type
get = Fold.paraM get'

-- | Gets the type of a term whose type does not depend upon the type of
-- free variables. Assumes the term is well-typed.
getClosed :: Term -> Type
getClosed = Env.empty . get

-- | Whether we can use the 'getClosed' function to type a term.
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
    get' t
    
    
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
    checkArgs (Prelude.get boundType x_b) (map snd xs)
    
check' (Def' name xs) = 
  checkArgs (Prelude.get nameType name) (map snd xs)
check' (Con' con xs) = 
  checkArgs (Type.constructorType con) (map snd xs)
  
check' (Case' (_, ind_ty) _)
  | not (Type.isInd ind_ty) = 
    Err.patternMatchNonInductive (show ind_ty)
    
check' (Case' (_, Base cse_ind) falts) =
  zipWithM_ checkAlt' [0..] falts
  where
  return_ty = snd (Prelude.get altTerm' (head falts))
  
  checkAlt' :: Nat -> Alt' (Term, Type) -> m ()
  checkAlt' alt_n (Alt' con@(Constructor ind con_n) bs (_, alt_ty))
    | ind /= cse_ind = Err.incorrectPattern (show con)
    | alt_n /= con_n = Err.patternsOutOfOrder
    | alt_ty /= return_ty = Err.incorrectAltType (show return_ty) (show alt_ty)
    | (length bs_tys :: Int) /= length arg_tys = Err.incorrectPattern (show con)
    | or (zipWith (/=) arg_tys bs_tys) = Err.incorrectPattern (show con)
    where
    (arg_tys, ind') = id
      . splitAt (length bs) 
      . Type.flatten
      $ Type.constructorType con
    bs_tys = map (Prelude.get boundType) bs
    
check' _ = 
  return ()
  
    
-- | Returns the type of a term, given the type of its subterms.
-- Assumes well-typedness.
get' :: Env.Bindings m => Term' (Term, Type) -> m Type
get' (Var' x xs) =
  liftM (Type.dropArgs (length xs) . Prelude.get boundType) (Env.boundAt x)
get' other =
  return (getClosed' other)
  
-- | Returns the type of a term, given the type of its subterms, without
-- appealing to a type environment. Cannot type variables.
getClosed' :: Term' (Term, Type) -> Type
getClosed' (Unr' ty) = ty
getClosed' (Lam' (Bind _ a) (_, b)) = Type.Fun a b
getClosed' (Def' name xs) = dropArgs (length xs) (Prelude.get nameType name)
getClosed' (Con' con xs) = dropArgs (length xs) (Type.constructorType con)
getClosed' (Case' _ (Alt' _ _ (_, ty) : _)) = ty

