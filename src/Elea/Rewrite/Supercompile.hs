module Elea.Rewrite.Supercompile (
  apply,
  findUnfolding
) where

import Elea.Prelude
import Elea.Term
import Elea.Monad.Direction ( Direction )
import qualified Elea.Type as Type
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Ext as Term
import qualified Elea.Term.Tag as Tag
import qualified Elea.Foldable as Fold
import qualified Elea.Foldable.WellFormed as WellFormed
import qualified Elea.Rewrite.Drive as Drive
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Data.Set as Set


type Env m = 
  ( Memo.Can m
  , Env.MatchRead m
  , Direction.Has m 
  , Tag.Gen m )


apply :: Env m => Term -> m Term
apply !term
  | not . isFix . leftmost $ term = return term
  | isFixPromoted term = return term
  | otherwise = return term


-- | (f, xs) = findUnfolding t ==> Term.reduce f xs == t /\ all isFix xs
findUnfolding :: Term -> (Term, [Term])
findUnfolding !term@(flattenApp -> fix@Fix {} : args)
  | all (not . any isFix . Term.appSubterms) args 
  || null arg_fix_calls =
    (Term.makeContext id (Term.toBind term), [term])
    -- ^ If there are no fixed-points in the term arguments,
    -- we can only unfold the outermost fixed-point

  | otherwise = 
    Assert.check 
      (mapM_ WellFormed.assert (new_ctx : new_fix_calls))
      (new_ctx, new_fix_calls) 
  where
  (new_ctx, new_fix_calls) = mergeUnfoldings pre_merged

  pre_merged :: (Term, [(Term, [Term])])
  pre_merged = ( Term.abstractTerms arg_fix_calls term
               , map findUnfolding arg_fix_calls )

  arg_fix_calls :: [Term]
  arg_fix_calls = id
    . filter (`Set.member` arg_subterms)
    . nubOrd
    . matchedFixCalls 
    $ unfolded_t
    where
    unfolded_t = Drive.inc $ App (Term.unfoldFix fix) args

    arg_subterms :: Set Term
    arg_subterms = id
      . Set.fromList 
      . concatMap Term.appSubterms 
      $ args

  matchedFixCalls :: Term -> [Term]
  matchedFixCalls (Case cse_of alts) 
    | isFix (leftmost cse_of) = cse_of : alt_matches
    | otherwise = alt_matches
    where
    alt_matches = concatMap matchedInAlt alts

    matchedInAlt :: Alt -> [Term]
    matchedInAlt Alt { _altBindings = binds, _altInner = alt_t } = id
      . catMaybes
      . map (Indices.tryLowerMany (nlength binds))
      $ matchedFixCalls alt_t
  matchedFixCalls _ = []

  mergeUnfoldings :: (Term, [(Term, [Term])]) -> (Term, [Term])
  mergeUnfoldings (outer_ctx, inner_unfoldings) =
    (foldr merge outer_ctx inner_unfoldings, concatMap snd inner_unfoldings)
    where
    merge :: (Term, [Term]) -> Term -> Term
    merge (inner_ctx, inner_fix_ts) full_ctx = id
      . unflattenLam inner_binds
      . (\f -> Term.reduce f [inner_app])
      . liftHere
      $ full_ctx
      where
      liftHere = Indices.liftMany (nlength inner_binds) 
      inner_binds = map Term.toBind inner_fix_ts
      inner_app = id
        . Term.reduce (liftHere inner_ctx)
        . Term.bindsToVars 
        $ inner_binds
