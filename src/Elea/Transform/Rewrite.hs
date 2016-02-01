-- | Simplifications which enable rewrites and which perform rewrites.
-- Needs heavy use of 'Elea.Embed'.
module Elea.Transform.Rewrite
(
  Step,
  applyM,
  rewriteSteps,
  expressSteps
)
where

import Elea.Prelude hiding ( run )
import Elea.Term
import Elea.Unification ( Unifier )
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Ext as Term
import qualified Elea.Type as Type
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Transform.Names as Name
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Prover as Prover
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable.WellFormed as WellFormed
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Fedd as Fedd
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Discovery as Discovery
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.StepCounter as Steps
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Error.Assertion as Assert

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Poset as Quasi

type Env m = 
  ( Defs.Read m
  , Discovery.Tells m
  , Env.All m
  , Tag.Gen m
  , History.Env m 
  , Fusion.Env m
  , Memo.Can m
  , Direction.Has m
  , Steps.Limiter m )

type Step m = (Env m, Prover.Step m)
  

applyM :: Env m => Term -> m Term
applyM = id
  . Transform.compose all_steps
  . WellFormed.check
  where
  all_steps = []
    ++ Eval.transformSteps
    ++ rewriteSteps
    ++ Eval.traverseSteps
    ++ expressSteps
    -- ^ Prioritising rewrites over descending into terms
    -- speeds things up a bit
    ++ Simp.steps
    ++ Prover.steps
    
    
rewriteSteps :: Step m => [(String, Term -> m Term)]
rewriteSteps =
  [ ("rewrite pattern", rewritePattern)
  , ("rewrite", rewrite) ] 

expressSteps :: Step m => [(String, Term -> m Term)]
expressSteps = 
  [ ("constructor fission", expressConstructor)
  , ("commute constraint", commuteConstraint)
  , ("accumulation fission", expressAccumulation)
  , ("unfold var match for fix-con fusion", matchVar)    
  , ("unfold finitely used fix", finiteCaseFix)
  , ("identity fission", identityFix) ] 
  

rewritePattern :: Step m => Term -> m Term
rewritePattern t = do
  t' <- Env.findMatch t
  Transform.continue t'
  -- ^ Might be variables in pattern which we can rewrite


rewrite :: forall m . Step m => Term -> m Term
rewrite term@(App {}) = do
  term' <- Term.revertMatches term
  rs <- Fusion.findTags (Set.delete Tag.omega (Tag.tags term'))
  Fail.choose (map (apply term') rs)
  where
  apply :: Term -> (Term, Term) -> m Term
  apply term (from_t, h) = do
    ms <- Env.matches
    args <- Term.findConstrainedArgs from_t term
    Fail.when (any isFinite args)
    return (app h args)
  
rewrite _ = Fail.here


matchVar :: forall m . Step m => Term -> m Term
matchVar term@(App fix@(Fix {}) xs) = do
  Fail.unless (Term.beingFused fix)  
  Direction.requireInc  
  [(from_f, to_f)] <- Fusion.findTags (Set.singleton tag)
  let from_t = snd (flattenLam from_f)
  id 
  --  . tracE [("match-var from", from_s), ("match-var with", t_s)]
    $ Fail.unless (Term.beingFused (leftmost from_t))
  -- ^ Make sure this is for a fixCon rewrite
  Fail.unless (length (Term.arguments from_t) == length xs) 
  Fail.choose (map (tryMatch from_t . enum) dec_idxs)
  where
  tag = get fixIndex (fixInfo fix)
  dec_idxs = Term.decreasingArgs fix
  
  tryMatch :: Term -> Int -> m Term
  tryMatch (App _ xs') arg_i = do
    Fail.unless (isCon (leftmost (xs' !! arg_i)))
    Fail.unless (isVar (xs !! arg_i))
    History.check Name.MatchVar new_t
      $ Transform.continue new_t
    where
    new_t = Term.buildCase (xs !! arg_i) term
    
matchVar _ = Fail.here


identityFix :: forall m . Step m => Term -> m Term
identityFix orig_t@(App fix@(Fix _ _ fix_t) xs) = do
  Direction.requireInc
  Fail.unless (worthATry fix_t) 
  Memo.memo Name.IdFix orig_t $ do    
    Fail.choose 
      . map tryArg
      . filter potentialArg
      $ [0..length xs - 1]
  where
  (arg_bs, body_t) = flattenLam fix_t
  (arg_tys, body_ty) = Type.split (Type.get fix)
  
  worthATry :: Term -> Bool
  worthATry (Lam _ t) = worthATry t
  worthATry (Case cse_t alts) = 
    worthATry cse_t && all (worthATry . get altInner) alts
  worthATry (Bot _) = True
  worthATry (flattenApp -> f : xs) = 
    (isCon f || isVar f) && all worthATry xs
  worthATry _ = False
  
  potentialArg :: Int -> Bool
  potentialArg n = 
    (arg_tys !! n) == Type.Base body_ty
  
  tryArg :: Int -> m Term
  tryArg n = do
    Prover.check (Leq (Indices.subst id_n fix_t) id_n)
    return (xs !! n)
    where       
    id_n = id
      . unflattenLam arg_bs 
      $ Var (enum ((length arg_bs - n) - 1)) (arg_bs !! n)
  
identityFix _ = Fail.here


expressConstructor :: forall m . Step m => Term -> m Term
expressConstructor term@(App fix@(Fix fix_i fix_b fix_t) args) = do
  Fail.when (Tag.tags term == Set.singleton Tag.omega) 
  Fail.when (Set.null suggestions)
  Fail.assert "express-constructor suggestions not correctly typed"
    $ all (== sugg_ty) sugg_tys
   -- ^ Check all the suggestions are correctly typed
    
  fix' <- id
    . Fail.choose 
    . map express 
    $ toList suggestions

  term' <- id
    . History.check Name.ExpressCon fix
    . Transform.continue
    $ app fix' args
    
  Fail.when (term Quasi.<= term')
  Discovery.rewritesTo term term'
  return term'
  where
  (arg_bs, _) = Term.flattenLam fix_t
  gap_b = Bind "gap" term_ty 
  term_ty = Type.get term
  sugg_ty = Type.Fun term_ty term_ty
  sugg_tys = map Type.get (Set.toList suggestions)
  
  sugg_ctxs = id
    . map suggestionToContext 
    $ Set.toList suggestions
    
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
          $ gapContext free_limit (enum arg_i)
      else return
         . Set.unions
         . map (Set.singleton . gapContext free_limit) 
         $ Type.recursiveArgs con
      where
      -- Construct a context which is the constructor term with
      -- a gap at the given argument position
      gapContext :: Nat -> Nat -> Term
      gapContext idx_offset gap_i = id
        . Lam gap_b
        . app (Con tcon)
        $ left ++ [Var 0 gap_b] ++ right
        where
        (left, _:right) = id
          . splitAt (enum gap_i) 
          $ Indices.lowerMany (pred idx_offset) args
            
    suggest term = do
      fix_f <- liftM pred Env.offset
      -- Fail if we've reached a return value, which is not in HNF,
      -- and also doesn't contain the recursive function call.
      Fail.unless (enum fix_f `Indices.freeWithin` term)
      return mempty

  suggestionToContext ctx_t = id
    . unflattenLam arg_bs
    . Term.reduce (Indices.liftMany (nlength arg_bs + 1) ctx_t)
    . (return :: a -> [a])
    . unflattenApp
    $ zipWith (Var . enum) (reverse [0..length arg_bs]) (fix_b : arg_bs)
      
  express :: Term -> m Term
  express sugg_t = id
    . liftM (\t -> Indices.subst (Fix fix_i fix_b t) ctx_comp)
    . Env.trackIndicesT sugg_t
    . Env.bind fix_b
    . strip
    . Simp.apply
    $ Indices.replaceAt 0 ctx_comp fix_t
    where
    ctx_comp = suggestionToContext sugg_t
    
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


-- | Unsound cos I'm an idiot
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
    if not (Indices.lowerableBy (enum idx_offset) cse_t) 
    then return Nothing
    else return 
       . Just
       . Case (Indices.lowerMany (enum idx_offset) cse_t)
       $ map (makeAlt idx_offset) alts
    where
    makeAlt offset alt@(Alt con bs _) = 
      Alt con bs (Term.replace cse_t' pat_t term')
      where
      pat_t = patternTerm (altPattern alt)
      term' = Indices.liftMany (nlength bs) term
      cse_t' = id
        . Indices.shift (\i -> (i - offset) + elength bs)
        $ cse_t
      
  freeCase _ = 
    return Nothing
    
expressMatch _ = Fail.here


commuteConstraint :: Step m => Term -> m Term
commuteConstraint term@(Case (leftmost -> Fix {}) _)
  | Constraint.splittable term
  , isCase inner_t 
  , not looping =
    Transform.continue (Case cse_t (map applyCt alts))
  where
  (ct, inner_t, ty) = Constraint.split term
  Case cse_t alts = inner_t
  looping = Constraint.has inner_t && isFix (leftmost inner_t)
  
  applyCt (Alt tc bs alt_t) = 
    Alt tc bs (Constraint.apply ty ct' alt_t)
    where
    ct' = Indices.liftMany (nlength bs) ct
  
commuteConstraint _ = Fail.here



-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
finiteCaseFix :: Step m => Term -> m Term
finiteCaseFix term@(Case cse_t@(App fix@(Fix _ _ fix_t) args) alts) = do
  -- I don't think this will ever usefully apply to non-recursive data types
  Fail.when (Term.beingFused fix)
  Fail.unless (Type.isRecursive cse_ind)
  Fail.unless (all finiteAlt alts)
 
  History.memoCheck Name.FiniteCaseFix term $ do
    term' <- id
      . Transform.continue 
      $ Case (Term.reduce (Term.unfoldFix fix) args) alts
    -- standard progress check
    Fail.when (term Quasi.<= term')
    return term'
  where  
  cse_ind = Type.fromBase (Type.get cse_t)
  
  -- A branch in which a recursive pattern variable is used
  finiteAlt :: Alt -> Bool
  finiteAlt (Alt tcon bs alt_t) =
    Set.null (Indices.free alt_t `Set.intersection` rec_vars)
    where
    con = Tag.untag tcon
    rec_vars = Set.fromList (Type.recursiveArgIndices con)
  
finiteCaseFix _ = Fail.here


expressAccumulation :: Step m => Term -> m Term
expressAccumulation fix@(Fix fix_i fix_b fix_t) = do
  Direction.requireDec
  Fail.unless (length acc_args == 1)
  -- ^ I am too rushed to code the general case atm
  Fail.unless (isJust mby_acc)
  Fail.unless (acc_idx `Indices.freeWithin` acc_t)  
  Fail.unless (Fold.any isCase body_t) 
  Fail.when (isVar acc_t)
  let acc_b = Bind "acc" (Type.get acc_t)
      trans_t =
        Indices.replaceAt (elength arg_bs) 
          (Term.reduce ctx_t_here [fun_var])
          body_t
  simp_t <- id
    . Assert.check (Type.assertEq body_t trans_t)
    . Env.bindMany (fix_b : arg_bs) 
    $ Transform.continue trans_t
  let replaced_t = Term.replace acc_t (Var Indices.omega acc_b) simp_t
  Fail.when (acc_idx `Indices.freeWithin` replaced_t)
  let new_fix = id
        . Fix fix_i fix_b 
        . unflattenLam arg_bs
        . Indices.replaceAt Indices.omega (Var acc_idx acc_b)
        $ replaced_t
  let final_t = Term.reduce ctx_t [new_fix]
  return final_t
  where
  acc_args = Term.accumulatingArgs fix
  [arg_n] = acc_args
  (arg_bs, body_t) = flattenLam fix_t
  acc_idx = (enum . pred) (nlength arg_bs - arg_n) :: Index
  fun_var = Var (elength arg_bs) fix_b
  arg_vars = id
    . setAt (enum arg_n) acc_t
    $ zipWith (Var . enum) (reverse (range arg_bs)) arg_bs
  Just acc_t = mby_acc
  ctx_t = id
    . unflattenLam (fix_b : arg_bs) 
    $ app fun_var arg_vars
  ctx_t_here = 
    Indices.liftMany (nlength (fix_b : arg_bs)) ctx_t
  
  mby_acc = guessAcc body_t  
    where
    guessAcc :: Term -> Maybe Term
    guessAcc cse_t@(Case _ alts) = do
      Fail.unless (length base_alts == 1)
      acc_t <- guessAcc alt_t
      Indices.tryLowerMany (nlength bs)  acc_t
      where
      base_alts = filter (Type.isBaseCase . Tag.untag . get altConstructor) alts
      [Alt _ bs alt_t] = base_alts
    guessAcc t = 
      return t
  
expressAccumulation _ = Fail.here


{-
unfoldCase :: Step m => Term -> m Term
unfoldCase term@(Case cse_t@(App fix@(Fix {}) xs) alts) = id
  . Memo.memo Name.UnfoldCase term 
  . History.check Name.UnfoldCase term $ do
      term' <- id
        . Transform.continue 
        $ Case (app (Term.unfoldFix fix) xs) alts
      when (term Quasi.<= term') $ do
      --  trace ("\n\n[failed case-unfold] " ++ show term ++ "\n\n[into] " ++ show term') 
          Fail.here
      return   
      -- $ trace ("\n\n[success case-unfold]" ++ show term   ++ "\n\n[into] " ++ show term')
        $ term'
  where
  strict_overlap = 
    Set.intersection (Term.strictAcross alts) 
                     (Term.strictWithin cse_t)
    
unfoldCase _ = Fail.here
-}

{-
expressConstraint :: Step m => Term -> m Term 
expressConstraint term@(App {}) = do
  ms <- liftM unfoldableMatches Env.matches
  
  try applying them, and checking for this:
  Fail.when (term Quasi.<= term')
  return term'
  
  where
  unfoldableMatches :: [Match] -> [Match]
  unfoldableMatches ms =
    filter (unfoldableFix . matchedTerm) ms
    where 
    unfoldableFix (App fix@(Fix {}) xs) =
      (not . Set.null . Set.intersection matched_vars) dec_xs
      where
      dec_xs = map (xs !!) (Term.decreasingArgs fix)
  
    matched_vars = id
      . Set.fromList
      . filter isVar
      . map matchedTerm 
      $ ms
      -}
