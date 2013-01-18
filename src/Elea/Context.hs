{-# LANGUAGE UndecidableInstances #-}
module Elea.Context 
(
  Context, make, apply, remove
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term ( Term (..), InnerTerm (..), Alt (..) )

import qualified Elea.Notes.Show as Show
import qualified Elea.Term as Term 
import qualified Elea.Monad.Failure as Fail
import qualified Data.Set as Set
import qualified Data.Label.Maybe as Maybe

newtype Context a = Context { _term :: Term a }
  deriving ( Eq, Ord )
  
mkLabels [''Context]

-- | This is a bit of a hack, but as long as no-one tries a 
-- 10000000 deep nesting of lambdas it should be fine :)
gapIndex :: Term.Index
gapIndex = toEnum 10000000

gap :: Show.HasNote a => Term a
gap = Show.tag "[_]" (Term empty (Var gapIndex))

make :: Show.HasNote a => (Term a -> Term a) -> Context a
make = Context . ($ gap)

apply :: Term.Notes a => Context a -> Term a -> Term a
apply = flip (Term.substAt gapIndex) . get term  

-- | If the given term is within the given context, then return
-- the value which has filled the context gap.
-- E.g. "remove (f [_] y) (f x y) == Just x" 
remove :: forall m a . (Show.HasNote a, Fail.Monad m) => 
  Context a -> Term a -> m (Term a)
remove (Context cxt_t) term = do
  terms <- execWriterT (rem cxt_t term)
  if Set.size terms /= 1
  then Fail.here
  else return $ head (Set.elems terms)
  where
  rem :: Term a -> Term a -> WriterT (Set (Term a)) m ()
  rem t1 t2 
    -- If t1 is a variable we get its index as "idx".
    | Just idx <- Maybe.get (Term.index . Term.inner) t1
    -- We need >= here as the gapIndex could have been 'lift'ed.
    , idx >= gapIndex = tell (Set.singleton t2)

    | otherwise = remI (get Term.inner t1) (get Term.inner t2)
   
  remI Absurd Absurd = return ()
  remI (Lam x) (Lam y) = rem x y
  remI (Fix x) (Fix y) = rem x y
  remI (App l1 r1) (App l2 r2) = do
    rem l1 l2
    rem r1 r2
  remI (Var x) (Var y) = Fail.when (x /= y)
  remI (Inj n1) (Inj n2) = Fail.when (n1 /= n2)
  remI (Case lhs1 alts1) (Case lhs2 alts2) = do
    Fail.when (length alts1 /= length alts2)
    rem lhs1 lhs2
    zipWithM remAlt alts1 alts2
    return ()
    where
    remAlt (Alt n1 t1) (Alt n2 t2) = do
      Fail.when (n1 /= n2)
      rem t1 t2
  remI _ _ = Fail.here 
     
instance (Term.Notes a, Show (Term a)) => Show (Context a) where
  show (Context term) = show term
    
  
