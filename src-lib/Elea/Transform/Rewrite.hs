-- | Simplifications which enable rewrites and which perform rewrites.
-- Needs heavy use of 'Elea.Embed'.
module Elea.Transform.Rewrite
(
  Step,
  rewriteSteps,
  expressSteps
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import Elea.Unification ( Unifier )
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Height as Height
import qualified Elea.Term.Ext as Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Transform.Names as Name
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
import qualified Data.Poset as Partial


type Step m = 
  ( Simp.Step m
  , Rewrite.Env m )
  
  
rewriteSteps :: Step m => [Term -> m Term]
rewriteSteps =
  [ const Fail.here
  , rewritePattern
  , rewrite
  ] 

expressSteps :: Step m => [Term -> m Term]
expressSteps = 
  [ const Fail.here
  , expressConstructor
  , expressMatch
  ] 
  

rewritePattern :: Step m => Term -> m Term
rewritePattern t = do
  t' <- Env.findMatch t
  Transform.continue t'
  -- ^ Might be variables in pattern which we can rewrite


rewrite :: forall m . Step m => Term -> m Term
rewrite term@(App {}) = do
  rs <- Rewrite.findTags (Set.delete Tag.omega (Tag.tags term))
  Fail.choose (map apply rs)
  where
  apply :: (Term, Index) -> m Term
  apply (from_t, h) = do
    ms <- Env.matches
    term_s <- showM term
    ms_s <- showM ms
    args <- id
     -- . trace ("\n\n[unifying] " ++ term_s
      --  ++ "\n\n[with] " ++ show from_t 
      --  ++ "\n\n[matches] " ++ ms_s) 
      $ Term.findConstrainedArgs from_t term
    return (app (Var h) args)
  
rewrite _ = Fail.here



expressConstructor :: forall m . Step m => Term -> m Term
expressConstructor term@(App fix@(Fix fix_i fix_b fix_t) args) = do
  Fail.when (Set.null suggestions)
  sugg_tys <- mapM Type.getM (Set.toList suggestions)
  Fail.assert "express-constructor suggestions not correctly typed"
    $ all (== sugg_ty) sugg_tys
    -- ^ Check all the suggestions are correctly typed
    
  ts <- showM term
  suggs <- showM suggestions 
    
  fix' <- id
    . Fail.choose 
    -- . trace ("\n\n[expr-con] " ++ ts ++ "\n\n[suggested] " ++ suggs)
    . map express 
    $ toList suggestions
    
  -- ts' <- showM (Eval.run (app fix' args))
    
  id
    . History.check Name.ExpressCon fix
    . Transform.continue
    $ app fix' args
  where
  (arg_bs, _) = Term.flattenLam fix_t
  gap_b = Bind "gap" term_ty 
  term_ty = Type.get term
  sugg_ty = Type.Fun term_ty term_ty
    
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
        Env.liftTrackedMany (nlength bs) (suggest alt_t)
        
    suggest con_t@(App (Con tcon@(Tag.untag -> con)) args) = do
      free_limit :: Nat <- liftM enum Env.tracked
      -- We cannot keep an argument to the constructor if it contains
      -- variables which are not free outside the fixpoint,
      -- because we cannot float these variables out.
      let not_keepable =
            findIndices (any (< enum free_limit) . Indices.free) args
            
      -- If we have more than one not keepable argument then none
      -- of them can be the gap, so we fail.
      Fail.when (length not_keepable > 1)
      
      let idx_offset = free_limit - nlength arg_bs
      
      if length not_keepable == 1
      then do
        -- If only one argument is not keepable, then we have to
        -- use this argument for the gap
        let arg_i = head not_keepable
        -- But this cannot be the gap if it's not recursive
        Fail.when (arg_i `elem` Type.nonRecursiveArgs con)
        return
          . Set.singleton
          $ gapContext idx_offset (enum arg_i)
      else return
         . Set.unions
         . map (Set.singleton . gapContext idx_offset) 
         $ Type.recursiveArgs con
      where
      -- Construct a context which is the constructor term with
      -- a gap at the given argument position
      gapContext :: Nat -> Nat -> Term
      gapContext idx_offset gap_i = id
        . Indices.lowerMany idx_offset
        . Lam gap_b
        . app (Con tcon)
        $ left ++ [Var 0] ++ right
        where
        (left, _:right) = splitAt (enum gap_i) args
            
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
    . Env.bind fix_b
    . strip
    . Simp.run
    $ Indices.replaceAt 0 ctx_comp fix_t
    where
    ctx_comp = id
      . unflattenLam arg_bs
      . app (Indices.liftMany (nlength arg_bs + 1) ctx_t)
      . (return :: a -> [a])
      . unflattenApp 
      . reverse 
      $ map (Var . enum) [0..length arg_bs]
    
    strip :: Term -> Env.TrackIndicesT Term m Term
    strip (Lam b t) = do
      t' <- Env.liftTracked (strip t)
      return (Lam b t')
    strip (Case t alts) =
      return (Case t) `ap` mapM stripAlt alts
      where
      stripAlt (Alt con bs t) = do
        t' <- Env.liftTrackedMany (nlength bs) (strip t)
        return (Alt con bs t')
    strip term = do
      sugg <- Env.tracked
      [arg] <- Term.findArguments sugg term
      return arg
        
expressConstructor _ = Fail.here


expressMatch :: Step m => Term -> m Term
expressMatch term@(App fix@(Fix {}) _) = do
  free_cse@(Case cse_t _) <- id
    . Fail.fromMaybe
    . Env.trackOffset
    $ Fold.findM freeCase fix
    
  History.check Name.ExpressMatch free_cse
    $ Transform.continue free_cse
  where
  freeCase :: Term -> Env.TrackOffset (Maybe Term)
  freeCase cse@(Case cse_t alts) = do
    idx_offset <- Env.tracked
    if any (< idx_offset) (Indices.free cse_t) 
    then return Nothing
    else return 
       . Just
       . Case (Indices.lowerMany (enum idx_offset) cse_t)
       $ map applyAlt alts
    where
    applyAlt (Alt con bs alt_t) = 
      Alt con bs (Indices.liftMany (nlength bs) term)
      
  freeCase _ = 
    return Nothing
    
expressMatch _ = Fail.here


