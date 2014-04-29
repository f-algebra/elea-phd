-- | The most basic and well established simplification steps,
-- plus a couple of functions which strongly rely on evaluation.
module Elea.Evaluation 
(
  run, steps,
  strictTerms,
  degenerateContext,
)
where

import Elea.Prelude
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Terms as Term
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Set as Set

run :: Term -> Term                           
run = Fold.rewriteSteps steps

steps :: Fail.Can m => [Term -> m Term]
steps = 
  [ unreachable
  , caseOfCon
  , caseCase
  , constantCase
  ]
  
  
-- | The variables or uninterpreted function application terms
-- whose value must be known in order to evaluate the given term.
strictTerms :: Defs.Read m => Term -> m (Set Term)
strictTerms (Case cse_t@(Def {}) _) = 
  strictTerms cse_t
strictTerms (Def name args) = do
  fun_t <- Defs.unfold name args
  return
    . collectTerms
    . run
    -- We replace this name with omega within its definition,
    -- to stop it getting unrolled again
    . Term.replaceName name Indices.omega
    $ fun_t
  where
  collectTerms (Case cse_t alts)
    | isVar cse_t = 
      Set.insert cse_t (concatMap altVars alts)
    where
    altVars (Alt _ bs alt_t) = id
      . Set.map (Indices.lowerMany (length bs))
      . Set.filter (Indices.lowerableBy (length bs))
      $ collectTerms alt_t 
  collectTerms _ = Set.empty
  
strictTerms _ = 
  return Set.empty
  
    
-- | Whether a context is degenerate down the current branch of a term.
-- A degenerate context is one which has lost its shape because we are down
-- a base case branch. For example @take n _@ is degenerate when @n = 0@.
-- This function is here because it requires 'strictVars'. 
degenerateContext :: (Defs.Read m, Env.Matches m) => Context -> m Bool
degenerateContext ctx = do
  strict <- strictTerms (Context.apply ctx (Var Indices.omega []))
  anyM Env.isBaseCase (toList strict)
    
  
-- | Finds terms that are unreachable and sets them that way.
-- Detects unreachable arguments to a function call, 
-- and pattern matching on an unreachable value.
unreachable :: Fail.Can m => Term -> m Term
unreachable term 
  | isUnr term && Type.isClosed term = 
    return (Unr (Type.getClosed term))
  where
  isUnr (Unr {}) = True
  isUnr (Case (Unr {}) _) = True
  isUnr (Con _ xs) = any isUnr xs
  isUnr (Def _ xs) = any isUnr xs
  isUnr (Var _ xs) = any isUnr xs
  isUnr _ = False
unreachable _ = Fail.here


caseOfCon :: Fail.Can m => Term -> m Term
caseOfCon (Case (Con con args) alts) = id
  -- Some assertions which should hold if the term is well formed
  . assert (con == alt_c)
  . assert (nlength args == length alt_bs)
  . return
  
  -- We fold substitute over the arguments to the constructor
  -- starting with the return value of the pattern match (alt_t).
  -- So we substitute each argument in one by one to the alt term.
  . foldr subst alt_t
  
  -- When we substitute an constructor argument, 
  -- it needs to not be affected by the substitution of later arguments.
  -- So we lift their indices a number of times
  -- depending on their position in the order of substitution, 
  -- viz. those substituted first are lifted the most.
  $ zipWith liftMany [0..] args
  where
  Alt alt_c alt_bs alt_t = 
    alts !! get Type.constructorIndex con

caseOfCon _ = Fail.here


-- | If we are pattern matching on a pattern match then remove this 
-- using distributivity.
caseCase :: Fail.Can m => Term -> m Term
caseCase outer_cse@(Case inner_cse@(Case {}) _) =
  return (Term.applyCase inner_cse outer_cse)
caseCase _ = Fail.here


-- | Removes a pattern match if every branch returns the same value.
constantCase :: forall m . Fail.Can m => Term -> m Term
constantCase (Case _ alts) = do
  (alt_t:alt_ts) <- mapM loweredAltTerm alts
  Fail.unless (all (== alt_t) alt_ts)
  return alt_t
  where
  loweredAltTerm :: Alt -> m Term
  loweredAltTerm (Alt _ bs alt_t) = 
    Indices.tryLowerMany (length bs) alt_t
    
constantCase _ = Fail.here


{-
-- | Moves all pattern matches over variables topmost in a term. 
-- Be careful with this, it can cause loops if combined with all sorts 
-- of things.
floatVarMatches :: Term -> Term
floatVarMatches = Fold.rewrite float . run
  where
  float :: forall m . Fail.Can m => Term -> m Term
  float outer_t@(Case _ (leftmost -> Fix {}) alts) = do
    inner_case <- Fail.choose (map caseOfVarAlt alts)
    return  
      . run
      $ Term.applyCase inner_case outer_t
    where
    caseOfVarAlt :: Alt -> m Term
    -- We return the inner alt case-of if it is over a variable
    -- which is not from the pattern match, viz. it can be lowered
    -- to outside the match.
    caseOfVarAlt (Alt bs alt_t@(Case ind cse_t i_alts)) 
      | isVar (leftmost cse_t) = do
        cse_t' <- Indices.tryLowerMany (length bs) cse_t
        return (Case ind cse_t' i_alts)
    caseOfVarAlt _ = 
      Fail.here
      
  float _ = Fail.here
-}

