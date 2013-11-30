module Elea.Context 
(
  Context, 
  make, apply,
  isConstant, gapType,
  toLambda, fromLambda,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Type ( Type, Bind (..) )
import qualified Elea.Index as Indices
import qualified Elea.Type as Type
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

make :: Type -> (Term -> Term) -> Context
make gap_ty mk_t = id
  . Context
  . Lam (Bind "[_]" gap_ty)
  . substAt Indices.omega (Var 0)
  . Indices.lift
  $ mk_t (Var Indices.omega)

apply :: Context -> Term -> Term
apply (Context (Lam _ rhs)) arg = subst arg rhs

toLambda :: Context -> Term
toLambda = get term

fromLambda :: Term -> Context
fromLambda = Context

gapType :: Context -> Type
gapType (Context (Lam (Bind _ gap_ty) _)) = gap_ty

-- | Returns whether there is any gap in the given context.
isConstant :: Context -> Bool
isConstant (Context (Lam _ t)) = 
  not (0 `Set.member` Indices.free t)

  {-
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
    . substAt 0 (Var Indices.omega)
    $ rhs_t
  
  merge :: Term -> Env.TrackIndices Index Term
  merge orig@(App (Var x1) (Var x2 : xs)) 
    | x2 == Indices.omega = do
      idx <- Env.tracked
      if x1 == idx 
      then return (app (Var idx) xs)
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
  then return (Absurd Set)
  else do
    let [(idx, hole_term)] = Map.toList uni
    Fail.when (idx /= 0)
    return (Indices.lower hole_term)

    -}
    
instance Indexed Context where
  free = Indices.free . get term
  shift f = modify term (Indices.shift f)

instance Substitutable Context where
  type Inner Context = Term
  substAt at with = modify term (substAt at with)
  
instance ContainsTerms Context where
  mapTermsM f = modifyM term f

