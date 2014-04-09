-- | Functions about types which require type environments and so cannot
-- be in Elea.Type. Notably the 'get' function which returns
-- the type of a term, and the 'check' function which checks a term
-- is correctly typed.
module Elea.Types
(
  module Elea.Type,
  Env,
  quickGet,
  get, check, checkStep, checkTrace,
)
where

import Prelude ()
import Elea.Prelude hiding ( get, mapM )
import Elea.Index hiding ( lift )
import Elea.Type
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Prelude as Prelude
import qualified Elea.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Failure.Class as Fail

-- A synonym for the constraints required on a monad so it can type a term
type Env m = (Defs.Read m, Env.Read m)

-- | Return the type of a term. Assumes the term is well-typed.
get :: Env m => Term -> m Type
get = Err.noneM . typeOf

-- | Return the type of a term if it can be found without an environment.
-- Assumes the term is well-typed.
-- Performs no type-checking at all.
-- Requires the result type to not depend upon the type of any free variables.
-- Right now it only works on fixpoints with or without arguments applied.
quickGet :: Term -> Type
quickGet (Fix _ (Bind _ ty) _) = ty
quickGet (App f args) = id
  . Type.unflatten 
  . drop (length args)
  . Type.flatten
  $ quickGet f


-- | Throws an error if a term is not correctly typed.
check :: (Err.Can m, Env m) => Term -> m ()
-- Call 'typeOf' and ignore the argument
check = liftM (const ()) . typeOf

checkTrace :: Env m => Term -> m a -> m a
checkTrace term m = Err.noneM (check term) >> m

-- | Wrap this around a term transformation step @Term -> m Term@
-- to add a check that the step preserves the type of the term.
checkStep :: forall m . Env m => 
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
  stepErr :: Term -> EitherT Err.Err m Err.Err
  stepErr result = do
    t_s <- showM term
    t_s' <- showM result
    Err.throw
      $ "In the transformation:"
      ++ "\nFrom: [" ++ t_s ++ "]"
      ++ "\nTo: [" ++ t_s' ++ "]"

-- | Returns the type of a given 'Term',
-- within a readable type environment.
-- Can throw type checking errors.
typeOf :: (Err.Can m, Env m) => Term -> m Type  
typeOf term = id
  . Err.augmentM (termErr term)
  . Fold.paraM doBoth
  $ term
  where
  -- This is added to the existing error 
  -- if a type checking error is thrown.
  termErr :: (Err.Can m, Env m) => Term -> m Err.Err
  termErr t = do
    t_s <- showM t
    bs <- Env.bindings
    let bs_s = id
          . intercalate " "
          $ map show bs
    return  
      $ "When type checking: [" ++ t_s 
      ++ "] \n\nWithin environment \n[" ++ bs_s ++ "]\n"
      
  -- Check the term for type errors, then return the type.
  doBoth :: (Err.Can m, Env m) => Term' (Type, Term) -> m Type
  doBoth ft = 
    Err.augmentM (termErr $ Fold.recover ft)
      $ fcheck ft >> ftype ft
    
  applyArg :: Term' (Type, Term) -> Term' (Type, Term)
  applyArg (App' (Type.Fun _ res_ty, f) ((_, t):ts)) =  
    App' (res_ty, app f [t]) ts
    
  -- Checks the term for any type errors.
  -- Is combined with 'ftype' in 'doBoth'
  -- to get the full monadic Term'-algebra for typing.
  fcheck :: (Err.Can m, Env m) => Term' (Type, Term) -> m ()

  -- Check that a variable has a type in the environment
  fcheck (Var' idx)
    | idx == Indices.omega = 
      Err.throw "The omega variable index cannot be typed."
  
  fcheck (Var' idx) = do
    depth <- Env.bindingDepth
    Err.when (fromEnum idx >= depth)
      $ "Found index: " ++ show idx ++ " in an environment which only "
      ++ "has type bindings up to index: " ++ show depth
      
  -- Check that term application has the correct function type on the left
  fcheck app_t@(App' (fun_ty, _) ((arg_ty, _) : args))
    | not (Type.isFun fun_ty) =
      Err.throw
        $ "Term application with non function typed [" 
        ++ show fun_ty ++ "] term leftmost."
      
    | Type.Fun arg_ty' _ <- fun_ty
    , arg_ty /= arg_ty' =
      Err.throw
        $ "Term application with incorrect argument type.\n"
        ++ "Expected [" ++ show arg_ty' ++ "]\n"
        ++ "Found [" ++ show arg_ty ++ "]" 
      
    | otherwise =
      fcheck (applyArg app_t)
      
  -- Check that a fixpoint matches the type of its bound variable
  fcheck (Fix' _ (Bind _ fix_ty) (fix_ty', _))
    | fix_ty /= fix_ty' = Err.throw
      $ "Fixpoint does not match its declared type."
      
  -- Check that an constructor is for an inductive type, and that the 
  -- constructor index is not greater than the number of constructors.
  fcheck (Con' ty n) 
    | length (Type.unfold ty) <= n = Err.throw
      $ "The given inductive type does not have that many constructors."
      
  -- Check that the pattern matched branches properly match the 
  -- constructors of the inductive type
  fcheck (Case' ind_ty (ind_ty', _) falts)
    | Type.Base ind_ty /= ind_ty' = Err.throw
      $ "Inductive type of pattern match [" ++ show ind_ty ++ "] does not"
      ++ " match type of matched term [" ++ show ind_ty' ++ "]"
      
    | length falts /= (length cons :: Int) = Err.throw 
      $ "Number of patterns does not match number of constructors."
      
    | not alts_correct = Err.throw 
      $ "Patterns matched do not match the constructors of the inductive type."
    where
    cons = Type.unfold ind_ty
    alts_correct = and . zipWith checkAlt cons $ falts
      where
      checkAlt (Bind _ con_ty) (Alt' bs (res_ty, _)) =
        con_ty == Type.unflatten (bs_tys ++ [ind_ty'])
        where
        bs_tys = map (Prelude.get boundType) bs
        
  fcheck _ = 
    return ()
 
  
  -- | Returns the type of a term, given the type of its subterms.
  ftype :: (Err.Can m, Env m) => Term' (Type, Term) -> m Type
  ftype (Absurd' ty) =
    return ty
  ftype (Var' idx) =
    liftM (Prelude.get boundType) (Env.boundAt idx)
  ftype (Lam' (Bind _ ty) (res, _)) = 
    return (Type.Fun ty res)
  ftype (App' (ty, _) []) = 
    return ty
  ftype t@(App' {}) = 
    ftype (applyArg t)
  ftype (Fix' _ (Bind _ ty) _) = 
    return ty
  ftype (Con' ind_ty n) =
    return
    . Prelude.get boundType
    . (`nth` enum n)
    $ Type.unfold ind_ty
  ftype (Case' _ _ (Alt' _ (ty, _) : _)) =
    return ty

