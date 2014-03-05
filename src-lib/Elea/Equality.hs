-- | This module is not used at the moment
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
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs

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
    eq' <- simplify eq
    eq_s <- showM eq'
    trace ("?=? " ++ eq_s) $ Env.alsoTrack 0 (Fold.foldM solveEquation eq')
    where
    args1' = map (Indices.liftMany 2) args1
    args2' = map (Indices.liftMany 2) args2
    
    term1 = app (Indices.liftAt 1 fix_t1) args1'
    term2 = app (Indices.lift fix_t2) args2'
    
    bindFixes :: m a -> m a
    bindFixes = Env.bind fix_b2 . Env.bind fix_b1
    
    solveEquation :: Term -> Env.AlsoTrack Index m Result
    solveEquation term
      | Just (left, right) <- Term.isEquation term = do
        offset <- Env.tracked
        let left' = id
              . Env.trackIndices offset
              $ Fold.rewriteM applyIndHyp left
        ls <- showM left'
        rs <- showM right
        let s = ls ++ " =?= " ++ rs
        trace s $ lift (equal left' right)
      where
      applyIndHyp :: Term -> MaybeT (Env.TrackIndices Index) Term
      applyIndHyp term@(App (Var f) _) = do
        f_var <- Env.tracked
        Fail.unless (f == f_var)
        uni_with <- Env.liftByOffset (app (Var 0) args1')
        uni <- Unifier.find term uni_with
        rep_with <- Env.liftByOffset (app (Var 1) args2')
        return (Unifier.apply uni rep_with)
      applyIndHyp _ =
        Fail.here
        
    solveEquation _ = 
      return mempty
  
  equal _ _ =
    return Unknown
    
