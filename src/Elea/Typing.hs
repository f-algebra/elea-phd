module Elea.Typing
(
  typeOf, check, absurd, empty
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Env as Env
import qualified Elea.Monad.Error as Err
import qualified Data.Set as Set

type TypingMonad m = (Err.Monad m, Env.Readable m)

-- | The type of 'Absurd', "all (a:*) -> a"
absurd :: Type
absurd = Pi (Bind (Just "FAIL") Set) (Var 0)

-- | The constructorless inductive type, "ind (0:*) with end"
empty :: Type 
empty = Ind (Bind (Just "0") Set) []

-- | Throws an error if a term is not correctly typed.
check :: TypingMonad m => Term -> m ()
-- 'Type' does not have a type in CoC, but is still correct
-- under type checking.
check Type = return ()
-- Otherwise we just see if 'typeOf' throws an error.
check other = liftM (const ()) (typeOf other)

-- | Returns the type of a given 'Term',
-- within a readable type environment.
-- Can throw type checking errors.
typeOf :: TypingMonad m => Term -> m Term  
typeOf term =
  Err.augmentM (termErr term)
  . Err.check check
  . Fold.paraM doBoth
  $ term
  where
  -- | This is added to the existing error 
  -- if a type checking error is thrown.
  termErr :: TypingMonad m => Term -> m Err.Err
  termErr t = do
    t_s <- showM t
    return $ "When type checking: [" ++ t_s ++ "]."
  
  -- | Check the term for type errors, then return the type.
  doBoth :: TypingMonad m => Term' (Type, Term) -> m Type
  doBoth ft = 
    Err.augmentM (termErr $ Fold.recover ft)
      $ fcheck ft >> ftype ft
      
  -- | The type of an pattern match branch has to be lowered by any
  -- indexes bound by the match
  altType :: Alt' (Type, Term) -> Type
  altType (Alt' bs (ty, t)) =
    Indices.lowerMany (length bs) ty
  
  -- | Checks the term for any type errors.
  -- Is combined with 'ftype' in 'doBoth'
  -- to get the full monadic Term'-algebra for typing.
  fcheck :: TypingMonad m => Term' (Type, Term) -> m ()
  fcheck Type' = Err.throw "[Type] has no type"
  fcheck (Var' idx) = do
    depth <- Env.bindingDepth
    Err.when (fromEnum idx >= depth)
      $ "Found index: " ++ show idx ++ " in an environment which only "
      ++ "has type bindings up to index: " ++ show depth
  fcheck (App' (fun_ty, fun_t) (arg_ty, arg_t))
    | Pi (Bind _ arg_ty') _ <- fun_ty
    , arg_ty /= arg_ty' = do
        ty_s <- showM arg_ty
        ty_s' <- showM fun_ty
        arg_s <- showM arg_t
        fun_s <- showM fun_t
        Err.throw 
          $ "Applying [" ++ fun_s ++ "]: [" ++ ty_s'
          ++ "]\nto [" ++ arg_s ++ "]: [" ++ ty_s 
          ++ "]\nArgument types do not match."
    | not (isPi fun_ty) = do
        ty_s <- showM fun_ty
        t_s <- showM fun_t
        Err.throw 
          $ "Found an applied term [" ++ t_s
          ++ "] of non-function type [" ++ ty_s ++ "]."       
  fcheck fx@(Fix' (Bind' _ (_, b_ty)) (Indices.lower -> ty, _))
    | b_ty /= ty = do
        b_ty_s <- showM b_ty
        ty_s <- showM ty
        fx_s <- showM (Fold.recover fx)
        Err.throw
          $ "In the fixpoint [" ++ fx_s ++ "].\n"
          ++ "The declared type [" ++ b_ty_s
          ++ "] and the type of the body [" ++ ty_s
          ++ "] do not match."
  fcheck (Inj' n (_, ind_ty))
    | not (isInd ind_ty) = do
        ty_s <- showM ind_ty
        Err.throw 
          $ "The type argument of an inj [" ++ ty_s
          ++ "] must be an inductive type."
    | fromEnum n >= num_cons = do
        ty_s <- showM ind_ty
        Err.throw 
          $ "Trying to inject into constructor " ++ show n
          ++ " of [" ++ ty_s ++ "] which only has " 
          ++ show num_cons ++ " constructors."
    where
    Ind _ (length -> num_cons) = ind_ty
  fcheck fcse@(Case' (ty, _) (_, arg_ty) falts) 
    | ty /= arg_ty = do
        ty_s <- showM ty
        arg_ty_s <- showM arg_ty
        Err.augmentM in_the_cse
          . Err.throw
          $ "The stored argument type [" ++ arg_ty_s
          ++ "] of a pattern match does not equal "
          ++ "the actual argument type [" ++ ty_s ++ "]."
    | Just fail_ty <- find (/= alt_ty) alt_tys = do
        fail_s <- showM fail_ty
        alt_s <- showM alt_ty
        Err.augmentM in_the_cse
          . Err.throw
          $ "The return types of two branches "
          ++ "are not equal: [" ++ alt_s ++ "] /= [" ++ fail_s ++ "]."
    where
    in_the_cse = do
      cse_s <- showM (Fold.recover fcse)
      return $ "In the pattern match {" ++ cse_s ++ "}.\n"
    alt_ty:alt_tys = map altType falts
  fcheck _ = 
    return ()
 
  
  -- | Returns the type of a term, given the type of its subterms.
  ftype :: TypingMonad m => Term' (Type, Term) -> m Type
  ftype Absurd' = 
    return absurd
  ftype Set' = 
    return Type
  ftype (Pi' _ _) = 
    return Set
  ftype (Var' idx) =
    liftM (get boundType) (Env.boundAt idx)
  ftype (Lam' (Bind' lbl (_, arg)) (res, _)) = 
    return (Pi (Bind lbl arg) res)
  ftype (App' (Pi _ res, _) (_, arg)) = 
    return (subst arg res)
  ftype (Fix' (Bind' _ (_, ty)) _) = 
    return ty
  ftype (Ind' (Bind' _ (_, ty)) _) = 
    return ty
  ftype (Inj' n (_, ind_ty)) =
    return
    . get boundType
    . (!! fromEnum n)
    . unfoldInd 
    $ ind_ty
  ftype (Case' _ _ falts) =
    return . altType . head $ falts

