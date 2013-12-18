module Elea.Context 
(
  Context, term,
  make, apply,
  isConstant, fromLambda,
  strip, dropLambdas,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Type ( Type, Bind (..) )
import qualified Elea.Index as Indices
import qualified Elea.Type as Type
import qualified Elea.Env as Env
import qualified Elea.Foldable as Fold
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A single hole term context 
newtype Context
  = Context { _term :: Term }
  deriving ( Eq, Ord )
  
mkLabels [''Context]

-- | Should be fairly obvious how contexts are equivalent to @Term -> Term@
-- but I can't think of a nice explanation to put here.
make :: (Term -> Term) -> Context
make mk_t = id
  . Context
  $ mk_t (Var Indices.omega)

-- | This is the 'make' function backwards
apply :: Context -> (Term -> Term)
apply (Context ctx_t) arg_t = 
  Indices.substAt Indices.omega arg_t ctx_t

fromLambda :: Term -> Context
fromLambda = 
  Context . Indices.substAt 0 (Var Indices.omega) . inner
  
-- | Returns whether there is any gap in the given context.
isConstant :: Context -> Bool
isConstant =
  not . Indices.containsOmega . get term
  
  
-- | Takes a context which has a lambda topmost
-- and drops it if that abstracted variable is always applied to the gap.
-- For example "\x -> f (_ x)" becomes just "f _".
-- If this transformation is not applicable then this function just returns
-- the original context.
dropLambdas :: Context -> Context
dropLambdas (Context (Lam lam_b lam_t))
  -- If this is 'Nothing' then there existed an instance of the gap
  -- which did not have the abstracted variable applied as its first argument.
  | Just lam_t' <- mby_lam_t'
  
  -- If the 0th index is not present then we have removed
  -- all instances of the lambda abstracted variable, so we can
  -- safely drop the lambda.
  , not (0 `Set.member` Indices.free lam_t') = 
      dropLambdas (Context (Indices.lower lam_t'))
  where
  mby_lam_t' = id
    . Env.trackIndices 0
    . runMaybeT
    . Fold.transformM merge
    $ lam_t
  
  -- Performs the transformation which removes instances of the variable
  -- applied to the gap term.
  merge :: Term -> MaybeT (Env.TrackIndices Index) Term
  merge term@(App (Var x1) (Var x2 : xs)) 
    | x1 == Indices.omega = do
      idx <- Trans.lift Env.tracked
      if x2 == idx 
      then return (app (Var Indices.omega) xs)
      -- Fail if we have an instance of the gap which does not
      -- have the lambda abstracted variable applied to it.
      else mzero
  merge other = 
    return other
    
dropLambdas other = other

    
-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
-- If this is a constant context then it returns 'Absurd' if it matches,
-- because obviously there is no gap to return when the context is stripped.
strip :: forall m . Fail.Can m => Context -> Term -> m Term
strip (Context ctx_t) term = do
  uni <- Unifier.find ctx_t term
  Fail.when (Map.size uni > 1)
  -- If the terms are equal then there is no gap,
  -- viz. we have unified with a constant context.
  if Map.size uni == 0
  then return (Absurd Type.empty)
  else do
    let [(idx, hole_term)] = Map.toList uni
    -- The only variable we should be replacing is the gap variable, 
    -- viz. omega
    Fail.when (idx /= Indices.omega)
    return hole_term
    
    
instance Indexed Context where
  free = Indices.free . get term
  shift f = modify term (Indices.shift f)

instance Indices.Substitutable Context where
  type Inner Context = Term
  substAt at with = modify term (Indices.substAt at with)
  
instance ContainsTerms Context where
  mapTermsM f = modifyM term f

