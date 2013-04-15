module Elea.Context 
(
  Context, make, apply, toLambda, fromLambda, remove
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Term
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

-- This is a bit of a hack, but as long as no-one tries a 
-- 10000000 deep nesting of bindings it should be fine :)
gapIndex :: Index
gapIndex = 10000000

gap :: Term
gap = Var gapIndex

make :: Type -> (Term -> Term) -> Context
make ty mk_t = Context (mk_t gap) ty

apply :: Context -> Term -> Term
apply = flip (substAt gapIndex) . get term  

toLambda :: Context -> Term
toLambda (Context term ty) = 
  Lam (Bind (Just "[_]") ty) inner_t
  where
  inner_t = 
      substAt (succ gapIndex) (Var 0) 
    $ Indices.lift term
    
fromLambda :: Show Term => Term -> Context
fromLambda (Lam (Bind _ ty) rhs) = 
  Context (subst gap rhs) ty

-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
remove :: forall m . Fail.Monad m => Context -> Term -> m Term
remove (Context cxt_t _) term = do
  uni <- Unifier.find cxt_t term
  Fail.when (Map.size uni /= 1)
  let [(idx, hole_term)] = Map.toList uni
  Fail.when (idx /= gapIndex)
  return hole_term
  
instance Liftable Context where
  liftAt at (Context t ty) = 
    Context (liftAt at t') (liftAt at ty)
    where
    t' = substAt gapIndex (lower gap) t
   
