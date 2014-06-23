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
  makeContext
)
where

import Elea.Prelude hiding ( replace )
import Elea.Term hiding ( apply )
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Terms as Term
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Failure.Class as Fail


make :: Inst Constructor -> Term -> Constraint
make = Constraint

-- | Remove a constraint from a term, returning the constraint, and the term
-- it was applied to. Fails if the term does not have a constraint topmost.
strip :: Fail.Can m => Term -> m (Constraint, Term)
strip (Case cse_t alts) = do
  Fail.unless (length non_abs == 1)
  inner_t <- Indices.tryLowerMany (length bs) alt_t'
  return (make con cse_t, inner_t)
  where
  non_abs = findIndices (not . isUnr . altTerm) alts
  [con_n] = non_abs
  Alt con bs alt_t = alts !! con_n
  
  -- Revert the pattern match
  cse_t' = Indices.liftMany (length bs) cse_t
  pat_t = Term.constructorPattern con
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
removeWhen check = Fold.transform remove
  where
  remove term
    | Just (con, inner_t) <- strip term, check con = inner_t
    | otherwise = term
      
    
-- | Composition of 'toContext' and 'make'
makeContext :: Term -> Inst Constructor -> Type -> Context
makeContext match_t con res_ty = 
  toContext res_ty (make con match_t)
  
  
-- | Pattern matches from "Elea.Monad.Env" are represented as term pairs, where
-- the first term has been matched to the second (which will always
-- be a constructor applied to variables).
fromMatch :: (Term, Term) -> Constraint
fromMatch (from_t, Con con _) = 
  make con from_t
  
  
-- | Composition of 'toContext' and 'fromMatch'
matchContext :: Type -> (Term, Term) -> Context
matchContext ty = toContext ty . fromMatch


-- | Applies the constraint to a term (with its type provided). 
-- Will capture any variables
-- the constraint pattern match captures.
-- Not the same as converting to a context and applying that, as that will
-- not capture variables.
apply :: Constraint -> Typed Term -> Term
apply (Constraint con match_t) (Typed on_t on_ty) =  
  Case match_t alts
  where
  alts = id
    . map buildAlt
    . sequence
    . fmap (Type.constructors . constructorOf)
    $ con
    
  con_n = constructorIndex (instObj con)
  
  buildAlt :: Inst Constructor -> Alt
  buildAlt alt_con@(Inst _ (Constructor ind alt_n)) = 
    Alt alt_con bs alt_t
    where
    bs = id
      . map (Bind "X") 
      . init 
      . Type.flatten 
      $ Type.get alt_con
    
    -- If we are not down the matched branch we return unreachable
    alt_t | alt_n /= con_n = Unr on_ty
          | otherwise = on_t

-- | Create a context from a constraint. Requires the return type of the gap.
toContext :: Type -> Constraint -> Context
toContext result_ty constr =
  Context.make (\gap_t -> apply constr (Type.specify gap_t result_ty))

