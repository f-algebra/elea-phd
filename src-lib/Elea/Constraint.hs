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
  Constraint (..),
  make,
  strip,
  target,
  is,
  removeAll,
  toContext,
  fuse,
  makeContext,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Simplifier as Simp
import qualified Elea.Fixpoint as Fix
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs


data Constraint 
  = Constraint  { matchTerm :: !Term
                , inductiveType :: !Type.Ind
                , constructorIndex :: !Nat }
                
make :: Term -> Type.Ind -> Nat -> Constraint
make = Constraint


-- | Remove a constraint from a term, returning the constraint, and the term
-- it was applied to.
strip :: Fail.Can m => Term -> m (Constraint, Term)
strip (Case ind cse_t alts)
  | length alts > 1
  , [con_n] <- findIndices (not . isAbsurd . get altInner) alts
  , lowerableAltInner (alts !! con_n) = 
    return ( make cse_t ind (enum con_n)
           , loweredAltInner (alts !! con_n) )
strip _ = 
  Fail.here
  
                
-- | The /target/ of a constraint is the term which is being constrained.
-- The value of the single non-absurd branch.
target :: Fail.Can m => Term -> m Term
target = liftM snd . strip

  
-- | Whether the given pattern match term represents a constraint.
is :: Term -> Bool
is = isJust . strip


-- | Removes any instances of constraints within a term.
-- Constraints on a term are always equivalent to just the term.
removeAll :: Term -> Term
removeAll = Fold.transform (\t -> fromMaybe t (target t))


-- | A function which composes 'make' and 'toContext'
makeContext :: Term -> Type.Ind -> Nat -> Type -> Context
makeContext match_t ind con_n res_ty = 
  toContext res_ty (make match_t ind con_n)
        
  
-- | Create a context from a constraint. Requires the return type of the gap.
toContext :: Type -> Constraint -> Context
toContext result_ty (Constraint match_t ind con_n) =
  Context.make makeContext
  where
  makeContext gap_t = 
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
      alt_t | enum alt_n /= con_n = Absurd result_ty
            | otherwise = gap_t
       
            
-- | Use fixpoint fusion to merge a constraint into a fixpoint.
fuse :: forall m . (Env.Read m, Defs.Read m, Fail.Can m)
  => Constraint 
  -> Term
  -> m Term
fuse cons@(Constraint match_t ind con_n) 
     term@(App fix@(Fix {}) args) = do
     
  -- Run fixpoint fusion with our special simplification function.
  Fix.fusion simplify full_ctx fix
  where
  -- We can use quickGet here because it's a fixpoint with arguments applied
  result_ty = Type.quickGet term
  
  -- Appending contexts composes them in the natural way
  cons_ctx = toContext result_ty cons
  args_ctx = Context.make (\t -> app t args)
  full_ctx = cons_ctx ++ args_ctx

  -- The inner simplification used in match fix fusion
  simplify :: Term -> m Term
  simplify term = do
    term' <- Simp.run term
    return
      . Env.trackIndices fix_f
      $ Fold.transformM expressPattern term'
    where
    fix_f = 0
    ctx = Indices.lift full_ctx
    orig_term = Context.apply ctx (Var fix_f)
    
    expressPattern :: Term -> Env.TrackIndices Index Term
    expressPattern (Case ind cse_t alts) 
      -- If we have found a pattern match which unifies with the original
      -- match context then we should express it at recursive calls to the
      -- function
      | Unifier.exists match_t cse_t = do
        fix_f <- Env.tracked
        let alt_t' = id
              . Env.trackIndices (fix_f, cse_t)
              . Env.liftTrackedMany (length alt_bs)
              $ Fold.transformM express alt_t
            alts' = l_alts ++ (Alt alt_bs alt_t' : r_alts)
        return (Case ind cse_t alts')
      where
      (l_alts, Alt alt_bs alt_t : r_alts) = splitAt (enum con_n) alts
      
      express :: Term -> Env.TrackIndices (Index, Term) Term
      express term@(App (Var f) args) = do
        (fix_f, cse_t) <- Env.tracked
        let cse_ctx = toContext result_ty (make cse_t ind con_n)
            term' = Context.apply cse_ctx term
        -- We only need to express the constraint if it can be unified with
        -- the original constraint, and hence could be replaced by fusion.
        if fix_f == f 
          && Unifier.exists orig_term term'
        then return term'
        else return term
      express other = 
        return other
        
    expressPattern other = 
      return other

fuse _ _ = 
  Fail.here
