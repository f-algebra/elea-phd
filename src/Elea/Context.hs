{-# LANGUAGE UndecidableInstances #-}
module Elea.Context 
(
  Context, make, apply, toLambda, fromLambda, remove
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import Elea.Type ( Type )
import Elea.Term ( Term (..), Alt (..) )
import qualified Elea.Index as Index
import qualified Elea.Term as Term 
import qualified Elea.Type as Type
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set

data Context 
  = Context   { _term :: Term
              , _gapType :: Type }
  deriving ( Eq, Ord )
  
mkLabels [''Context]

-- | This is a bit of a hack, but as long as no-one tries a 
-- 10000000 deep nesting of lambdas it should be fine :)
gapIndex :: Index
gapIndex = 10000000

gap :: Term
gap = Term.Var gapIndex

make :: Type -> (Term -> Term) -> Context
make ty mk_t = Context (mk_t gap) ty

apply :: Context -> Term -> Term
apply = flip (substAt gapIndex) . get term  

toLambda :: Context -> Term
toLambda (Context term ty) = 
  Term.Lam (Type.Bind (Just "[_]") ty) inner_t
  where
  inner_t = 
      substAt (succ gapIndex) (Var 0) 
    $ Index.lift term
    
fromLambda :: Term -> Context
fromLambda (Lam (Type.Bind _ ty) rhs) = 
  Context (subst gap rhs) ty

-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
remove :: forall m . Fail.Monad m => Context -> Term -> m Term
remove (Context cxt_t _) term = do
  terms <- execWriterT (rem cxt_t term)
  if Set.size terms /= 1
  then Fail.here
  else return $ head (Set.elems terms)
  where
  rem :: Term -> Term -> WriterT (Set Term) m ()
  rem (Var idx) t2 
    | idx >= gapIndex = tell (Set.singleton t2)
  rem Absurd Absurd = return ()
  rem (Lam b1 t1) (Lam b2 t2) = 
    assert (b1 == b2) (rem t1 t2)
  rem (Fix b1 t1) (Fix b2 t2) = 
    assert (b1 == b2) (rem t1 t2)
  rem (App l1 r1) (App l2 r2) = do
    rem l1 l2
    rem r1 r2
  rem (Type ty1) (Type ty2) =
    assert (ty1 == ty2) (return ())
  rem (Var x) (Var y) = 
    Fail.when (x /= y)
  rem (Inj n1 ty1) (Inj n2 ty2) = 
      assert (ty1 == ty2)
    $ Fail.when (n1 /= n2)
  rem (Case lhs1 _ alts1) (Case lhs2 _ alts2) = do
    Fail.when (length alts1 /= length alts2)
    rem lhs1 lhs2
    zipWithM (rem `on` get Term.altInner) alts1 alts2
    return ()
  remI _ _ = Fail.here 
    
  
