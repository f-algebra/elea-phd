module Elea.Context 
(
  Context,
  pushVar, localVar,
  lookupType, askType,
  typeOf
)
where

import Prelude ()
import Elea.Prelude
import Elea.Type ( Type (..) )
import Elea.Term ( Term (..) )
import Elea.Equation ( Equation (..) )

import qualified Elea.Type as Type
import qualified Elea.Term as Term

data Context 
  = Context { types :: [Type]
            , facts :: [Equation] }

pushVar :: Type -> Context -> Context
pushVar ty ctx =
  ctx { types = ty : types ctx
      , facts = map Term.lift (facts ctx) }

localVar :: MonadReader Context m => Type -> m a -> m a
localVar = local . pushVar

lookupType :: Term.Index -> Context -> Type
lookupType idx = (!! fromEnum idx) . types

askType :: MonadReader Context m => Term.Index -> m Type
askType idx = asks (lookupType idx)

-- | Return the 'Type' of a given 'Term' within a 'Context'
typeOf :: forall m . MonadReader Context m => Term -> m Type
typeOf (Term.Var idx) = askType idx
typeOf (Abs ty) = return ty
typeOf (Inj n ty@(Ind tys)) =
  return $ Type.unflatten ((tys !! n) ++ [ty])
typeOf (App lhs rhs) = do
  Arr arg_ty res_ty <- typeOf lhs
  arg_ty' <- typeOf rhs
  assert (arg_ty == arg_ty')
    $ return res_ty
typeOf (Lam arg_ty rhs) =     
  localVar arg_ty $ do
    rhs_ty <- typeOf rhs
    return (Arr arg_ty rhs_ty)
typeOf (Fix ty rhs) = 
  localVar ty $ do
    fix_ty <- typeOf rhs
    assert (fix_ty == ty)
      $ return fix_ty
typeOf (Case term alts) = do
  Ind tys <- typeOf term
  (res_ty:other_tys) <- 
    assert (length alts == length tys)
    $ zipWithM typeOfAlt tys alts
  assert (all (== res_ty) other_tys)
    $ return res_ty
  where
  typeOfAlt :: [Type] -> Term -> m Type
  typeOfAlt arg_tys alt = do 
    alt_ty <- Type.flatten `liftM` typeOf alt
    let (arg_tys', res_tys) = 
          splitAt (length arg_tys) alt_ty
    return 
      $ assert (and $ zipWith (==) arg_tys arg_tys')
      $ Type.unflatten res_tys

