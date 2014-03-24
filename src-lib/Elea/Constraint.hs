-- | A constraint is a pattern match where only one branch is non-absurd.
-- It represents a constraint on the inputs to the non-absurd branch 
-- (the constrained term). 
-- > match leq_nat x y with
-- > | True -> x + y
-- > | False -> _|_
-- > end
-- The above is the term @x + y@, with the value of @x@ constrained to be
-- less-than-or-equal-to the value of @y@.
module Elea.Constraint
(
  make,
  strip,
  target,
  is,
  contains,
  fromMatch,
  removeAll,
  removeWhen,
  apply,
  toContext,
  matchContext,
  makeContext,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Terms as Term
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs

                
make :: Term -> Type.Ind -> Nat -> Constraint
make = Constraint


-- | Remove a constraint from a term, returning the constraint, and the term
-- it was applied to.
strip :: Fail.Can m => Term -> m (Constraint, Term)
strip (Case ind cse_t alts) = do
  Fail.unless (length non_abs == 1)
  inner_t <- Indices.tryLowerMany (length bs) alt_t'
  return (make cse_t ind (enum con_n), inner_t)
  where
  non_abs = findIndices (not . isAbsurd . get altInner) alts
  [con_n] = non_abs
  Alt bs alt_t = alts !! con_n
  
  -- Revert the pattern match
  cse_t' = Indices.liftMany (length bs) cse_t
  pat_t = Term.altPattern ind (enum con_n)
  alt_t' = Term.replace pat_t cse_t' alt_t
    
strip _ = 
  Fail.here
  
                
-- | The /target/ of a constraint is the term which is being constrained.
-- The value of the single non-absurd branch.
target :: Fail.Can m => Term -> m Term
target = liftM snd . strip

  
-- | Whether the given pattern match term represents a constraint.
is :: Term -> Bool
is = isJust . strip

-- | Whether a given term contains constraints
contains :: Term -> Bool
contains = Fold.any is

-- | Removes any instances of constraints within a term.
-- Constraints on a term are always equivalent to just the term.
removeAll :: Term -> Term
removeAll = removeWhen (const True)

-- | Strips all constraints from a term which fulfil the given predicate
removeWhen :: (Constraint -> Bool) -> Term -> Term
removeWhen p = Fold.transform remove
  where
  remove term
    | Just (con, inner_t) <- strip term, p con = inner_t
    | otherwise = term
      
    
-- | Composition of 'toContext' and 'make'
makeContext :: Term -> Type.Ind -> Nat -> Type -> Context
makeContext match_t ind con_n res_ty = 
  toContext res_ty (make match_t ind con_n)
  
  
-- | Pattern matches from "Elea.Monad.Env" are represented as term pairs, where
-- the first term has been matched to the second (which will always
-- be a constructor applied to variables).
fromMatch :: (Term, Term) -> Constraint
fromMatch (from_t, leftmost -> Con ind con_n) = 
  make from_t ind con_n
  
  
-- | Composition of 'toContext' and 'fromMatch'
matchContext :: Type -> (Term, Term) -> Context
matchContext ty = toContext ty . fromMatch


-- | Applies the constraint to a term (with its type provided). 
-- Will capture any variables
-- the constraint pattern match captures.
-- Not the same as converting to a context and applying that, as that will
-- not capture variables.
apply :: Constraint -> (Term, Type) -> Term
apply (Constraint match_t ind con_n) (on_t, on_ty) =  
  Case ind match_t alts
  where
  cons = Type.unfold ind
  alts = map buildAlt [0..length cons - 1]
  
  buildAlt :: Int -> Alt
  buildAlt alt_n = 
    Alt bs alt_t
    where
    Bind _ con_ty = id
      . assert (length cons > alt_n)
      $ cons !! alt_n
    bs = map (Bind "X") (init (Type.flatten con_ty))
    
    -- If we are not down the matched branch we return absurd
    alt_t | enum alt_n /= con_n = Absurd on_ty
          | otherwise = on_t

-- | Create a context from a constraint. Requires the return type of the gap.
toContext :: Type -> Constraint -> Context
toContext result_ty constr =
  Context.make (\gap_t -> apply constr (gap_t, result_ty))

