-- | Type and name (text labels for variables) contexts
module Elea.Context 
(
  Context,
  pushTyVar, pushVar,
  localTyVar, localVar,
  lookupType, askType,
  typeOf
)
where

import Prelude ()
import Elea.Prelude hiding ( All )
import Elea.Type ( Type )
import Elea.Term ( Term (..) )

import qualified Elea.Type as Type
import qualified Elea.Term as Term

data Context 
  = Context { tyVars :: [Text]
            , vars :: [(Text, Type)] }
       
pushTyVar :: Text -> Context -> Context
pushTyVar label ctx =
  ctx { tyVars = label : tyVars ctx }

pushVar :: Text -> Type -> Context -> Context
pushVar label ty ctx =
  ctx { vars = (label, ty) : vars ctx }

localTyVar :: MonadReader Context m => Text -> m a -> m a
localTyVar = local . pushTyVar

localVar :: MonadReader Context m => Text -> Type -> m a -> m a
localVar label = local . pushVar label

lookupType :: Term.Index -> Context -> Type
lookupType idx = snd . (!! fromEnum idx) . vars

askType :: MonadReader Context m => Term.Index -> m Type
askType idx = asks (lookupType idx)

-- | Return the 'Type' of a given 'Term' within a 'Context'
typeOf :: MonadReader Context m => Term -> m Type
typeOf (Var idx) = 
  askType idx
typeOf (App lhs rhs) = do
  Type.Arr arg_ty res_ty <- typeOf lhs
  arg_ty' <- typeOf rhs
  return 
    $ assert (arg_ty == arg_ty')
    $ res_ty
typeOf (Lam label arg_ty rhs) =     
  localVar label arg_ty $ do
    rhs_ty <- typeOf rhs
    return (Type.Arr arg_ty rhs_ty)
typeOf (TyApp lhs arg_ty) = do
  Type.All _ res_ty <- typeOf lhs
  return (Type.substOutermost arg_ty res_ty)
typeOf (TyLam label rhs) =
  localTyVar label (typeOf rhs)
typeOf (In mu) =
  return (Type.Arr (Type.unfoldLfp mu) mu)
typeOf (Out mu) = 
  return (Type.Arr mu (Type.unfoldLfp mu))

