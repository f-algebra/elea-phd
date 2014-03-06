-- | An equation solver which uses proof by induction.
module Elea.Equality
(
  prove
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Unifier as Unifier
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Map as Map

-- | This is like 'Maybe' 'Bool' but with a distinctive monoid instance.
-- It characterises a proof in which all parts must be true for it to be true,
-- but only one part need be false for it to be false.
data Result 
  = Proven
  | Disproven
  | Unknown
  
instance Monoid Result where
  mempty = Proven
  
  Proven    `mappend` Proven    = Proven
  Disproven `mappend` _         = Disproven
  _         `mappend` Disproven = Disproven
  _         `mappend` _         = Unknown
  
resultToBool :: Fail.Can m => Result -> m Bool
resultToBool Unknown = Fail.here
resultToBool Proven = return True
resultToBool Disproven = return False
  
      
-- | Checks (using induction) whether two terms are equal.
prove :: forall m . (Env.Read m, Defs.Read m, Fail.Can m)
  -- | A simplification function to be called within proving.
  => (Term -> m Term)
  -> Term
  -> Term
  -> m Bool
prove simplify t1 t2 = do
  t1' <- simplify t1
  t2' <- simplify t2
  eq <- equal t1' t2'
  resultToBool eq
  where 
  equal :: Term -> Term -> m Result

  -- If both sides are constructors check they are the same constructor and that
  -- every argument is equal
  equal 
      (flattenApp -> Con _ n : l_args) 
      (flattenApp -> Con _ m : r_args) = do
    if n /= m
    then return Disproven
    else do
      args_eq <- zipWithM equal l_args r_args
      -- Because constructors are injective we can use the concatenate
      -- of our injective equality to conjoin the equality of each argument.
      return (mconcat args_eq)
  
  -- If one side is constructor shaped and the other isn't
  -- then we cannot decide equality.
  equal (leftmost -> Con {}) _ = return Unknown
  equal _ (leftmost -> Con {}) = return Unknown
  
  equal t1 t2
    | t1 == t2 = return Proven
  
  -- If both sides are the application of a function
  -- we can unroll those functions and check equality inductively
  equal
      (flattenApp -> Fix _ fix_b1 fix_t1 : args1) 
      (flattenApp -> Fix _ fix_b2 fix_t2 : args2) = bindFixes $ do
    eq <- Term.equation term1 term2
    let rewr_eq = id
          . Env.trackIndices (rewr1, rewr2)
          . Fold.rewriteM applyIndHyp
          $ Simp.run eq
    if Indices.freeWithin 0 rewr_eq
    -- If the fix variable of the left equation is still free then
    -- apply the inductive hypothesis has failed.
    then do
      eq_s <- showM rewr_eq
      id
        . trace ("\n[prove eq] failed on: " ++ eq_s) 
        $ return Unknown
    else do
      simp_eq <- simplify rewr_eq
      eq_s <- showM simp_eq
      id
        . trace ("\n[prove eq] checking: "++ eq_s) 
        $ Fold.foldM solveEquation simp_eq
    where
    args1' = map (Indices.liftMany 2) args1
    args2' = map (Indices.liftMany 2) args2
    
    -- The terms on either side of the equation, with the fixpoints
    -- unwrapped (not unrolled, the fix variables will be uninterpreted)
    term1 = app (Indices.liftAt 1 fix_t1) args1'
    term2 = app (Indices.lift fix_t2) args2'
    
    -- The terms we will be rewriting from and to when 
    -- we apply the inductive hypothesis
    rewr1 = app (Var 0) args1'
    rewr2 = app (Var 1) args2'
    
    bindFixes :: m a -> m a
    bindFixes = Env.bind fix_b2 . Env.bind fix_b1
    
    -- Unifies with instances of the lhs fix variable and replaces then
    -- with the rhs fix variable.
    applyIndHyp :: Term -> MaybeT (Env.TrackIndices (Term, Term)) Term
    applyIndHyp term@(App (Var f) _) = do
       (rewr1@(App (Var fix_f) _), rewr2) <- Env.tracked
       Fail.unless (f == fix_f)
       uni <- Unifier.find rewr1 term
       return (Unifier.apply uni rewr2)
    applyIndHyp _ = 
      Fail.here
      
    solveEquation :: Term -> m Result
    solveEquation term
      | Just (left, right) <- Term.isEquation term =
        equal left right
    solveEquation _ = 
        return mempty
  
  equal _ _ =
    return Unknown
    
