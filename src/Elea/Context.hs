module Elea.Context 
(
  Context, 
  make, apply, strip,
  toLambda, fromLambda, 
  dropLambdas,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure as Fail
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A single hole term context
newtype Context 
  -- | We store contexts as lambda abstractions
  = Context { _term :: Term }
  deriving ( Eq, Ord )
  
mkLabels [''Context]

make :: Type -> Type -> (Term -> Term) -> Context
make gap_ty ret_ty mk_t = id
  . Context
  . Lam lam_b
  . substAt Indices.omega (Var 0)
  . Indices.lift
  $ mk_t (Var Indices.omega)
  where
  lam_b = id
    . Bind (Just "[_]")
    . Pi (Bind (Just "[_]") gap_ty)
    $ Indices.lift ret_ty

apply :: Context -> Term -> Term
apply (Context (Lam _ rhs)) t = subst t rhs

fromLambda :: Term -> Context
fromLambda = Context

toLambda :: Context -> Term
toLambda = get term

-- | This is an odd function, it takes a context which has a lambda topmost
-- and drops it if that abstracted variable is always applied to the gap.
-- For example "\x -> f (_ x)" becomes just "f _".
-- Used for context splitting.
dropLambdas :: Context -> Context
dropLambdas (Context 
    (Lam ctx_b@(get boundType -> Pi _ rhs_ty) 
      (Lam rhs_b rhs_t)))
  | not (0 `Set.member` Indices.free rhs_ty)
  , not (Indices.omega `Set.member` Indices.free rhs_t') = 
    dropLambdas (Context (Lam ctx_b' rhs_t'))
  where
  ctx_b' = set boundType (Indices.lower rhs_ty) ctx_b
  
  rhs_t' = id
    . Env.trackIndices 0
    . Fold.transformM merge
    . substAt 1 (Var Indices.omega)
    $ rhs_t
  
  merge :: Term -> Env.TrackIndices Index Term
  merge orig@(App (Var x1) (Var x2)) 
    | x1 == Indices.omega = do
      idx <- ask
      if x2 == idx 
      then return (Var idx)
      else return orig
  merge other = 
    return other
dropLambdas other = other


-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
-- If this is a constant context then it returns 'Absurd' if it matches,
-- because obviously there is no gap to return when the context is stripped.
strip :: forall m . Fail.Monad m => Context -> Term -> m Term
strip (Context (Lam _ ctx_t)) term = do
  uni <- Unifier.find ctx_t (Indices.lift term)
  Fail.when (Map.size uni > 1)
  if Map.size uni == 0
  then return Absurd
  else do
    let [(idx, hole_term)] = Map.toList uni
    Fail.when (idx /= 0)
    return (Indices.lower hole_term)

instance Liftable Context where
  liftAt at (Context t) = Context (liftAt at t)

instance Substitutable Context where
  type Inner Context = Term
  substAt at with = modify term (substAt at with)
  free = Indices.free . get term
   
