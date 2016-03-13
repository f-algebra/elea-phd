module Elea.Rewrite.Supercompile (
  Action (..),
  apply,
  nextAction,
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
import qualified Data.Poset as Homeo


type Env m = 
  ( Memo.Can m
  , Direction.Has m 
  , Tag.Gen m )


apply :: Env m => Term -> m Term
apply !term
  | not . isFix . leftmost $ term = return term
  | isFixPromoted term = return term
  | otherwise = return term


data Action 
  = CaseSplit !Term
  | Induction !Term
  | Unfold !Term
  | Rewrite !Term !Term
  deriving ( Eq, Show )


nextAction :: Direction.Has m => Term -> m Action
nextAction !term@(flattenApp -> fix@Fix {} : args)
  | isFixPromoted term = return $ Unfold term
  | otherwise = do 
    term' <- Drive.rewrite (App (Term.unfoldFix fix) args)
    case term' of 
      Case { caseOf = cse_of }
        | not (Term.isStrictSubterm cse_of term) ->
          return $ CaseSplit cse_of
        | Just cse_of' <- findRewrite cse_of ->
          return $ Rewrite cse_of cse_of'
        | not . isFix . leftmost $ cse_of ->
          return $ Induction cse_of
        | otherwise -> 
          nextAction cse_of

      not_case -> 
        return $ Unfold term


findRewrite :: Fail.Can m => Term -> m Term
findRewrite _ = Fail.here

