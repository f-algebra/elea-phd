module Elea.Typing
(
  Kind (..), fromKind,
  typeOf, kindOf,
  checkType, checkTerm,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term ( Term' (..), Term, Alt' (..) )
import Elea.Type ( Bind' (..), Type' (..), Type (..), Bind (..) )
import Elea.Show ( showM )
import qualified Elea.Type as Type
import qualified Elea.Term as Term
import qualified Elea.Foldable as Fix
import qualified Elea.Monad.Error as Err

type TypingMonad m = (Err.Monad m, Type.ReadableEnv m)

-- | The kind of a regular type is a kind;
-- the kind of a kind is a 'Sort'.
data Kind 
  = Kind !Type 
  | Sort
  
fromKind :: Kind -> Type
fromKind (Kind ty) = ty

isSort :: Kind -> Bool
isSort Sort = True
isSort _ = False

isKind :: Kind -> Bool
isKind = not . isSort

-- | Throws an error if given an invalid type.
checkType :: TypingMonad m => Type -> m ()
checkType = liftM (const ()) . kindOf

-- | Throws an error if a term is not correctly typed.
checkTerm :: TypingMonad m => Term -> m ()
checkTerm = liftM (const ()) . typeOf

-- | Returns the 'Kind' of a given type. 
-- Can throw errors if given an invalid type.
-- The kind of a kind is a 'Sort'.
kindOf :: TypingMonad m  => Type -> m Kind
kindOf ty = return (Kind Type.Set) {-Fix.cataM fkind
  where
  more_err = do
    ty_s <- showM ty
    return $ "When kind checking: [" ++ ty_s ++ "]."
    
  -- | Check the type for errors, then return the kind
  doBoth ty = fcheck ty >> fkind ty
    
  -- | Check for any errors in the type.
  -- fcheck (Ind' b cons) =
  -- ... need to implement a check for proper inductive types
  fcheck (Fun' (Bind' _ (Kind _)) Sort) =
    Err.throw "Kinds cannot have a type arguments."
  fcheck (App' k1 k2) 
    | isSort k1 || isSort k2 = 
      Err.throw "Kinds cannot be used in type application."
  fcheck (App' (Kind fun_k) _) =
    | not (Type.isFun fun_k) =
      
    
  -- | Find the kind of the given type, 
  -- expressed as a monadic 'Type''-algebra.
  fkind :: TypingMonad m => Type' Kind -> m Kind
  fkind (Fun' _ Sort) = return Sort
  fkind -}
  
{-
kindOf (Fun (Bind lbl arg) res)
  | isKind arg = Fun (Bind lbl arg) (kindOf res)
  | otherwise = assert (not (isKind res)) Set
kindOf (App fun arg) = 
  assert (arg_k == arg_k') res_k
  where
  arg_k = kindOf arg
  Fun (Bind _ arg_k') res_k = kindOf fun
-}

-- | Returns the 'Type' of a given 'Term',
-- within a readable type environment.
-- Can throw type checking errors.
typeOf :: TypingMonad m => Term -> m Type  
typeOf term =
  Err.augmentM (termErr term)
    . Err.check checkType
    . Term.ignoreFacts 
    . Fix.paraM doBoth
    $ term
  where
  -- | This is added to the existing error 
  -- if a type checking error is thrown.
  termErr :: TypingMonad m => Term -> m Err.Err
  termErr t = do
    t_s <- showM t
    return $ "When type checking: {" ++ t_s ++ "}."
  
  -- | Check the term for type errors, then return the type.
  doBoth :: (TypingMonad m, Term.Facts m) => Term' (Type, Term) -> m Type
  doBoth ft = 
    Err.augmentM (termErr $ Fix.recover ft)
      $ fcheck ft >> ftype ft
      
  -- | The type of an pattern match branch has to be lowered by any
  -- indexes bound by the match
  altType :: Alt' (Type, Term) -> Type
  altType (Alt' bs (ty, _)) = 
    lowerMany (length bs) ty
  
  -- | Checks the term for any type errors.
  -- Is combined with 'ftype' in 'doBoth'
  -- to get the full monadic Term'-algebra for typing.
  fcheck :: TypingMonad m => Term' (Type, Term) -> m ()
  fcheck (Term.App' (fun_ty, fun_t) (arg_ty, arg_t))
    | Fun (Bind _ arg_ty') _ <- fun_ty
    , arg_ty /= arg_ty' = do
        ty_s <- showM arg_ty
        ty_s' <- showM fun_ty
        arg_s <- showM arg_t
        fun_s <- showM fun_t
        Err.throw 
          $ "Applying {" ++ fun_s ++ "}: [" ++ ty_s'
          ++ "]\nto {" ++ arg_s ++ "}: [" ++ ty_s 
          ++ "]\nArgument types do not match."
  fcheck (Term.App' (fun_ty, fun_t) _) 
    | not (Type.isFun fun_ty) = do
        ty_s <- showM fun_ty
        t_s <- showM fun_t
        Err.throw 
          $ "Found an applied term {" ++ t_s
          ++ "} of non-function type [" ++ ty_s ++ "]."
  fcheck (Type' ty)
    | Type.isKind ty = do
        ty_s <- showM ty
        Err.throw  
          $ "Attempted to apply a kind [" ++ ty_s 
          ++ "] in term position (types are allowed, just not kinds)."
  fcheck fx@(Fix' b (lower -> ty, _))
    | b_ty /= ty = do
        b_ty_s <- showM b_ty
        ty_s <- showM ty
        fx_s <- showM (Fix.recover fx)
        Err.throw
          $ "In the fixpoint {" ++ fx_s ++ "}.\n"
          ++ "The declared type [" ++ b_ty_s
          ++ "] and the type of the body [" ++ ty_s
          ++ "] do not match."
    where
    b_ty = get Type.boundType b
  fcheck (Inj' n ind_ty@(Ind _ cons)) 
    | not (Type.isInd ind_ty) = do
        ty_s <- showM ind_ty
        Err.throw 
          $ "The type argument of an inj [" ++ ty_s
          ++ "] must be an inductive type."
  fcheck fcse@(Case' (ty, _) arg_ty falts) 
    | ty /= arg_ty = do
        ty_s <- showM ty
        arg_ty_s <- showM arg_ty
        Err.augmentM in_the_cse
          . Err.throw
          $ "The stored argument type [" ++ arg_ty_s
          ++ "] of a pattern match does not equal "
          ++ "the actual argument type [" ++ ty_s ++ "].\n"
          ++ "This should be an assertion error I think, I'm not sure "
          ++ "how a user could cause this."
    | Just fail_ty <- find (/= alt_ty) alt_tys = do
        fail_s <- showM fail_ty
        alt_s <- showM alt_ty
        Err.augmentM in_the_cse
          . Err.throw
          $ "The return types of two branches of a pattern match "
          ++ "are not equal: [" ++ alt_s ++ "] /= [" ++ fail_s ++ "]."
    where
    in_the_cse = do
      cse_s <- showM (Fix.recover fcse)
      return $ "In the pattern match {" ++ cse_s ++ "}.\n"
    alt_ty:alt_tys = map altType falts
  fcheck _ = 
    return ()
  
  -- | A monadic Term'-algebra which finds the type of a term.
  ftype :: TypingMonad m => Term' (Type, Term) -> m Type
  ftype (Type' ty) =
    liftM fromKind (kindOf ty)
  ftype Absurd' =
    return Type.absurd
  ftype (Term.Var' idx) = 
    liftM (get Type.boundType) (Type.boundAt idx)
  ftype (Term.App' (Fun _ ret_ty, _) (_, Term.Type arg_ty)) =
    return (subst arg_ty ret_ty)
  ftype (Term.App' (Fun _ ret_ty, _) _) =
    return (lower ret_ty)
  ftype (Fix' b _) = 
    (return . get Type.boundType) b
  ftype (fmap fst -> Lam' b ty) =
    return (Type.Fun b ty)
  ftype (Inj' n (Type.unfoldInd -> cons)) = 
    return . get Type.boundType $ cons !! fromEnum n
  ftype (Case' _ _ falts) =
    (return . altType . head) falts

