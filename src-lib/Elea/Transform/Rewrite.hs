-- | Simplifications which enable rewrites and which perform rewrites.
-- Needs heavy use of 'Elea.Embed'.
module Elea.Rewrite
(
  Step,
  steps
)
where

import Elea.Prelude
import Elea.Term
import Elea.Unification ( Unifier )
import qualified Elea.Height as Height
import qualified Elea.Embed as Embed
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Rewrite as Rewrite

type Step m = 
  ( Transform.Step m
  , Embed.History m
  , Env.MatchRead m
  , Rewrite.Env m )
  
  
steps :: Step m => [Term -> m Term]
steps =
  [ const Fail.here
  , Height.assertDecrease "rewrite-pattern" rewritePattern
  , Height.assertDecrease "rewrite-function" rewriteFunction
  , Height.enforceDecrease unfold ] 

  
unfold :: Step m => Term -> m Term
unfold term@(App fix@(Fix {}) args) = do
  any_matched <- anyM Env.isMatched dec_args
  -- No point unrolling unless a decreasing argument has a constructor
  -- topmost, or has been matched to a constructor
  Fail.unless (any (isCon . leftmost) dec_args || any_matched)
  Embed.check term
    . Transform.continue
    $ app (Term.unfoldFix fix) args
  where
  dec_args = map (args !!) (Term.decreasingArgs fix)
unfold _ = Fail.here


rewritePattern :: Step m => Term -> m Term
rewritePattern (Case cse_t alts)  = do
  cse_t' <- Env.findMatch cse_t
  -- Re-use the evaluation step which reduces case-of over constructors
  Eval.caseOfCon cse_t'
rewritePattern _ = Fail.here


rewriteFunction :: Step m => Term -> m Term
rewriteFunction term@(App {}) = do
  rs <- Rewrite.findTags (Tags.tags term)
  Fail.choose (map apply rs)
  where
  apply :: Fail.Can m => (Term, Var) -> m Term
  apply (from_t, h) = do
    uni <- Unifier.find from_t term
    Fail.unless (Indices.free uni == arg_idxs)
    -- ^ Check that the list of indices we need to rewrite in order to unify
    -- matches those that are the arguments to the rhs of the rewrite
    let h_args = map snd (Map.toAscList uni)
    return (app (Var h) h_args)
    where
    (arg_bs, from_body) = flattenLam from_t
    arg_idxs = Set.fromList [0..nlength arg_bs - 1]
  
rewriteFunction _ = Fail.here



