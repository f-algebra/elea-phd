module Elea.Context 
(
  Context, make, apply, toLambda, fromLambda, strip
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure as Fail
import qualified Data.Map as Map

data Context 
  = Context   { _term :: Term
              , _gapType :: Type }
  deriving ( Eq, Ord )
  
mkLabels [''Context]

gap :: Term
gap = Var Indices.omega

make :: Type -> (Term -> Term) -> Context
make ty mk_t = Context (mk_t gap) ty

apply :: Context -> Term -> Term
apply = flip (substAt Indices.omega) . get term  

toLambda :: Context -> Term
toLambda (Context term ty) = 
  Lam (Bind (Just "[_]") ty) inner_t
  where
  inner_t = 
      substAt Indices.omega (Var 0) 
    $ Indices.lift term
    
fromLambda :: Show Term => Term -> Context
fromLambda (Lam (Bind _ ty) rhs) = 
  Context (subst gap rhs) ty

-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
strip :: forall m . Fail.Monad m => Context -> Term -> m Term
strip (Context cxt_t _) term = do
  uni <- Unifier.find cxt_t term
  Fail.when (Map.size uni /= 1)
  let [(idx, hole_term)] = Map.toList uni
  Fail.when (idx /= Indices.omega)
  return hole_term
  
instance Liftable Context where
  liftAt at (Context t ty) = 
    Context (liftAt at t') (liftAt at ty)
    where
    t' = substAt Indices.omega gap t
    
instance Substitutable Context where
  type Inner Context = Term

  substAt at with = id
    . modify term (substAt at with)
    . modify gapType (substAt at with)
    
  free = Indices.free . get term
   
