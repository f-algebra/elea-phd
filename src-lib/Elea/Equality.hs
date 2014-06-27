-- | An equation solver which uses proof by induction.
module Elea.Equality
(
  prove
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Failure.Class as Fail
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
  
  -- debug
  debug_eq <- Term.equation t1 t2
  eq_s <- showM debug_eq
  let s1 = "\n[prove eq] checking:\n" ++ eq_s
  
  eq <- id
    -- debug
   -- . trace s1
    $ equal t1' t2'
  resultToBool eq
  where 
  equal :: Term -> Term -> m Result

  -- If both sides are constructors check they are the same constructor and that
  -- every argument is equal
  equal 
      (flattenApp -> Con c1 : l_args) 
      (flattenApp -> Con c2 : r_args) = do
    if c1 /= c2
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
      (flattenApp -> Fix _ fix1_b fix1_t : args1) 
      (flattenApp -> fix2@(Fix {}) : args2) = Env.bind fix1_b $ do
    eq <- Term.equation term1 term2
    let rewr_eq = id
          . Env.trackIndices (hyp_t, term2)
          . Fold.rewriteM applyIndHyp
          $ Simp.run eq
    if Indices.freeWithin 0 rewr_eq
    -- If the fix variable of the left equation is still free then
    -- apply the inductive hypothesis has failed.
    then do
      eq_s <- showM rewr_eq
      id
        -- . trace ("\n[prove eq] failed on: " ++ eq_s) 
        $ return Unknown
    else do
      eq_s <- showM rewr_eq
      id
       -- . trace ("\n[prove eq] checking: "++ eq_s) 
        $ Fold.foldM solveEquation rewr_eq
    where
    args1' = Indices.lift args1
    args2' = Indices.lift args2
    
    -- The terms on either side of the equation, with the fixpoints
    -- unwrapped (not unrolled, the fix variables will be uninterpreted)
    term1 = app fix1_t args1'
    term2 = app (Indices.lift fix2) args2'
    
    -- The term we will be rewriting from when we apply
    -- the inductive hypothesis
    hyp_t = app (Var 0) args1'
    
    -- Unifies with instances of the lhs fix variable and replaces then
    -- with the rhs fix variable.
    applyIndHyp :: Term -> MaybeT (Env.TrackIndices (Term, Term)) Term
    applyIndHyp term1@(App (Var f) _) = do
       (rewr1@(App (Var fix_f) _), term2) <- Env.tracked
       Fail.unless (f == fix_f)
       uni <- Unifier.find rewr1 term1
       return (Unifier.apply uni term2)
    applyIndHyp _ = 
      Fail.here
      
    solveEquation :: Term -> m Result
    solveEquation term
      | Just (left, right) <- Term.isEquation term = do
        left' <- simplify left
        right' <- simplify right
        equal left' right'
    solveEquation _ = 
        return mempty
  
  equal _ _ =
    return Unknown
    
