-- | Simplifications which enable rewrites and which perform rewrites.
-- Needs heavy use of 'Elea.Embed'.
module Elea.Transform.Rewrite
(
  Step,
  steps
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import Elea.Unification ( Unifier )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Height as Height
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Monad.Failure.Class as Fail

import qualified Data.Map as Map
import qualified Data.Set as Set


type Step m = 
  ( Eval.Step m
  , Env.All m
  , Defs.Read m
  , Rewrite.Env m )
  
  
steps :: Step m => [Term -> m Term]
steps =
  [ const Fail.here
  , rewritePattern
  , rewriteFunction
  , expressConstructor
  , unfold
  ] 

  
unfold :: Step m => Term -> m Term
unfold term@(App fix@(Fix {}) args) =
  History.check "unf" term $ do
    any_matched <- anyM Env.isMatched dec_args
    -- No point unrolling unless a decreasing argument has a constructor
    -- topmost, or has been matched to a constructor
    Fail.unless (any (isCon . leftmost) dec_args || any_matched)
    term' <- Transform.continue (app (Term.unfoldFix fix) args)
    Height.ensureDecrease term' term
    return term'
  where
  dec_args = map (args !!) (Term.decreasingArgs fix)
  
unfold _ = Fail.here


rewritePattern :: Step m => Term -> m Term
rewritePattern (Case cse_t alts)  = do
  cse_t' <- Env.findMatch cse_t
  -- Re-use the evaluation step which reduces case-of over constructors
  Eval.caseOfCon cse_t'
  
rewritePattern _ = Fail.here


rewriteFunction :: forall m . Step m => Term -> m Term
rewriteFunction term@(App {}) = do
  rs <- Rewrite.findTags (Tag.tags term)
  Fail.choose (map apply rs)
  where
  apply :: Fail.Can m => (Term, Index) -> m Term
  apply (from_t, h) = do
    args <- id
      -- . trace ("\n\n[unifying]" ++ show term ++ "\n\n[with]" ++ show from_t)
      $ Term.findArguments from_t term
    return (app (Var h) args)
  
rewriteFunction _ = Fail.here


expressConstructor :: forall m . Step m => Term -> m Term
expressConstructor term@(App fix@(Fix fix_i fix_b fix_t) args) = do
  Fail.when (Set.null suggestions)
  Fail.assert 
    . all ((== suggestion_ty) . Type.get)
     $ Set.toList suggestions
    -- ^ Check all the suggestions are correctly typed
    
  fix' <- id
    . Fail.choose 
    . map express 
    $ toList suggestions
    
  ts <- showM term
  suggs <- showM suggestions 
  ts' <- showM (Eval.run (app fix' args))
    
  id
   -- . trace ("\n\n[expr-con] " ++ ts ++ "\n\n[suggested] " ++ suggs ++ "\n\n[result]" ++ ts')
    . History.check "exp-con" fix
    . Transform.continue 
    . Eval.run
    $ app fix' args
  where
  (arg_bs, _) = Term.flattenLam fix_t
  gap_b = Bind "X" term_ty 
  term_ty = Type.get term
  suggestion_ty = Type.Fun term_ty term_ty
    
  suggestions :: Set Term
  suggestions = id
    . fromMaybe mempty
    . Env.trackOffset
    . Env.liftTracked
    . runMaybeT
    $ suggest fix_t
    where
    suggest :: Term -> MaybeT Env.TrackOffset (Set Term)
    suggest (Lam _ t) = 
      Env.liftTracked (suggest t)
    suggest (Case _ alts) =
      concatMapM suggestAlt alts
      where
      suggestAlt :: Alt -> MaybeT Env.TrackOffset (Set Term)
      suggestAlt (Alt _ bs alt_t) =
        Env.liftTrackedMany (length bs) (suggest alt_t)
        
    suggest con_t@(App (Con con) args) = do
      free_limit <- Env.tracked
      -- We cannot keep an argument to the constructor if it contains
      -- variables which are not free outside the fixpoint,
      -- because we cannot float these variables out.
      let not_keepable = 
            findIndices (any (< free_limit) . Indices.free) args
            
      -- If we have more than one not keepable argument then none
      -- of them can be the gap, so we fail.
      Fail.when (length not_keepable > 1)
      
      let idx_offset = enum (free_limit - length arg_bs)
      
      if length not_keepable == 1
      then do
        -- If only one argument is not keepable, then we have to
        -- use this argument for the gap
        let arg_i = head not_keepable
        -- But this cannot be the gap if it's not recursive
        Fail.when (arg_i `elem` Type.nonRecursiveArgs con)
        return
          . Set.singleton
          $ gapContext idx_offset arg_i
      else return
         . Set.unions
         . map (Set.singleton . gapContext idx_offset) 
         $ Type.recursiveArgs con
      where
      -- Construct a context which is the constructor term with
      -- a gap at the given argument position
      gapContext :: Nat -> Int -> Term
      gapContext idx_offset gap_i = id
        . Indices.lowerMany idx_offset
        . Lam gap_b
        . app (Con con)
        $ left ++ [Var 0] ++ right
        where
        (left, _:right) = splitAt gap_i args
            
    suggest term = do
      fix_f <- liftM pred Env.offset
      -- Fail if we've reached a return value, which is not in HNF,
      -- and also doesn't contain the recursive function call.
      Fail.unless (enum fix_f `Indices.freeWithin` term)
      return mempty

      
  express :: Term -> m Term
  express ctx_t = id
    . liftM (\t -> Indices.subst (Fix fix_i fix_b t) ctx_comp)
    . Env.trackIndicesT ctx_t
    . strip
    . Simp.run
    $ Indices.replaceAt 0 ctx_comp fix_t
    where
    ctx_comp = id
      . unflattenLam arg_bs
      . app (Indices.liftMany (length arg_bs) ctx_t)
      . (return :: a -> [a])
      . unflattenApp 
      . reverse 
      $ map (Var . enum) [0..length arg_bs]
    
    strip :: Term -> Env.TrackIndicesT Term m Term
    strip (Lam b t) = 
      return (Lam b) `ap` strip t
    strip (Case t alts) =
      return (Case t) `ap` mapM stripAlt alts
      where
      stripAlt (Alt con bs t) = 
        return (Alt con bs) `ap` strip t
    strip term = do
      sugg <- Env.tracked
      mby_arg <- Fail.catch (Term.findArguments sugg term)
      case mby_arg of
        Just [arg] -> return arg
        Nothing 
          | App fix@(Fix {}) xs <- term -> 
            History.check "strip" term
              . strip
              . Simp.run
              $ app (Term.unfoldFix fix) xs
          
          | otherwise -> Fail.here
        
    
      
expressConstructor _ = Fail.here
