-- | The two functions which arise from the formula for fixpoint fusion.
-- If we represent the fusion equation as @C[fix F] = fix G@ then, 
-- 'fusion' takes @C@, @fix F@ and returns @fix G@,
-- 'fission' takes @C@, @fix G@ and returns @fix F@.
-- Also a function for fusing constraints into a term. I put it here
-- to prevent cycles.
module Elea.Fixpoint
(
  fusion, fission, 
  constraintFusion,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Monad.Fedd ( Fedd )
import qualified Elea.Unification as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Evaluation as Eval
import qualified Elea.Simplifier as Simp
import qualified Elea.Context as Context
import qualified Elea.Constraint as Constraint
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Memo.Class as Memo
import qualified Data.Map as Map
import qualified Data.Set as Set

{-# SPECIALISE fusion 
    :: (Term -> MaybeT Fedd Term)
    -> Context 
    -> Term  
    -> MaybeT Fedd Term #-}
  
{-# SPECIALISE fission 
  :: (Term -> MaybeT Fedd Term) 
  -> Term 
  -> Context 
  -> MaybeT Fedd Term #-}

{-# SPECIALISE constraintFusion
  :: (Term -> MaybeT Fedd Term) 
  -> Constraint 
  -> Term
  -> MaybeT Fedd Term #-}

-- | Fixpoint fusion.
-- > if E' = fusion C E
-- > then C[E] = E'
fusion :: forall m . (Fail.Can m, Env.All m, Defs.Read m, Memo.Can m) 
  -- | A simplification function to be called during fusion.
  -- All indices will have been lifted by one in the supplied term,
  -- and the index of the unrolled fixpoint variable will be 0.
  => (Term -> m Term)
  -> Context 
  -> Term 
  -> m Term
fusion _ ctx ifix 
  | not . isFix . leftmost $ ifix = do
    ifs <- showM ifix
    ctxs <- showM ctx
    error (ctxs ++ " with " ++ ifs)
fusion simplify outer_ctx inner_fix@(Fix fix_info fix_b fix_t) = 
    -- Use the fusion memoisation monad to memoise this computation
    Memo.fusion outer_ctx inner_fix $ do
  
  -- DEBUG
  ctx_s <- showM outer_ctx
  t_s <- showM inner_fix
  let s1 = "\nFUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s

  -- Make the type of the new fixpoint we are creating
  result_ty <- Type.getM orig_t
  free_var_bs <- mapM Env.boundAt free_vars
  let free_var_tys = map (get Type.bindType) free_var_bs
      new_fix_ty = Type.unflatten (reverse (result_ty:free_var_tys))
      new_lbl = "[" ++ get Type.bindLabel fix_b ++ "]"
      new_fix_b = Bind new_lbl new_fix_ty
  
  simplified_t <- id
    -- DEBUG
   . trace s1
  
    -- After simplification we replace all of the free
    -- variables within the term with fresh new ones,
    -- to be captured by the lambdas are new function will have.
    . liftM rebindVars
    
    -- Finally, run the given simplification function on it.
    . Env.bind fix_b
    . simplify
    
    -- Move all pattern matches on variables topmost to make sure everything
    -- unrolls properly
  --  . traceMe "\n\n!!!!!! YIELDED"
    . Eval.floatVarMatches
  --  . traceMe "\n\n?????? FLOATING VARS FROM"
    -- Apply the context to the unwrapped function body
    $ Context.apply (Indices.lift outer_ctx) fix_t
  
  -- Before we can replace we need to revert any pattern matches
  -- over the recursive function call, 
  -- as this will rewrite the term we are replacing
  let reverted_t = id
        -- DEBUG
        . Env.trackIndices 0
        . Term.revertMatchesWhenM isInnerFixMatch
        $ simplified_t
  
  -- DEBUG
  simp_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM reverted_t)
  let s2 = "\nSIMPLIFIED++:\n" ++ simp_s
  
  -- The new fix body with every occurrence of the context composed
  -- with the old fix variable replaced with the new fix variable
  let inner_fix_here = Indices.liftMany (length free_vars + 1) inner_fix
      replaced_t = id 
        . trace s2
        -- Expand every call to the inner fixpoint if it uses a finite
        -- amount of information from that fixpoint
        . expandFiniteCalls Indices.omega inner_fix_here
  
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Env.trackOffset
        . Env.trackMatches
        . Fold.rewriteM replace
        
        -- We replace all occurrences of the old fix variable with omega
        . Indices.substAt 0 (Var Indices.omega)
        $ reverted_t
        
  -- Our shiny new function
  let new_fix_body = id
        -- Put the recursive calls back in
        . Indices.substAt Indices.omega (Indices.lift inner_fix)
        . Term.unflattenLam (reverse free_var_bs) 
        $ replaced_t
      new_fix =
        -- We keep the fixpoint information of the inner fixpoint.
        -- So far it only stored fused matches, and these will continue to
        -- be applicable after a fusion step (I'm pretty sure...).
        Fix fix_info new_fix_b new_fix_body
        
  -- DEBUG
  let new_term = id
        . Simp.run
        . app new_fix 
        . map Var 
        $ reverse free_vars
  
  -- Fusion has failed if we do not recurse as much as the original function,
  -- or all recursive occurrences have been removed.
  -- Seems to be a good heuristic, but this is just evidence based.
  let old_rc = Set.size (functionCalls (Var 0) fix_t)
      new_rc = Set.size (functionCalls (Var 0) new_fix_body)
      rem_rc = id
        . Env.trackOffset
        . Env.trackMatches
        $ Fold.countM fixpointCall replaced_t
 
  rep_s <- showM new_term
  let s3 = "\nREPLACED:" ++ rep_s 
        
  Fail.unless                                                      
    -- DEBUG
    . trace s3
   -- . trace (s1 ++ s2 ++ s3)
    $ rem_rc == 0 
    || new_rc >= old_rc 
    
    -- Couple of extra ad hoc reasons fusion would have succeeded.
    || Term.isUnr new_term
    || Term.isSimple new_term
  
  -- DEBUG
  final_s <- showM new_term
  let s4 = "\nDONE:\n" ++ final_s
  return   
     . trace s4 
    $ new_term
  
  where
  orig_t = Context.apply outer_ctx inner_fix
  free_vars = id
    . sort
    . toList 
    $ Indices.free orig_t
  new_vars = map (Var . toEnum) [0..length free_vars - 1]
  rebound_outer_ctx = (Indices.lower . rebindVars . Indices.lift) outer_ctx
  
  isInnerFixMatch :: Term -> Env.TrackIndices Index Bool
  isInnerFixMatch (App (Var f) _ ) = do
    f_var <- Env.tracked
    return (f == f_var)
  isInnerFixMatch _ = return False
  
  -- Replace all free variables with fresh variables which
  -- will be bound by the lambdas of the new function
  -- we are creating
  rebindVars :: (Substitutable a, Inner a ~ Term) => a -> a
  rebindVars = id
    . concatEndos 
    . reverse
    . zipWith Indices.replaceAt (map Indices.lift free_vars)
    $ map Indices.lift new_vars
    
  -- A hacky attempt to detect the number of unique times a function
  -- is called within a term. Could underestimate.
  functionCalls :: Term -> Term -> Set Term
  functionCalls func = id
    . Env.trackIndices func
    . Fold.foldM call
    where
    call :: Term -> Env.TrackIndices Term (Set Term)
    call term@(App func' _) = do
      func <- Env.tracked
      if func' /= func
      then return mempty
      else do
        let min = minimum (Indices.free term)
        return (Set.singleton (Indices.lowerMany (enum min) term))
    call _ = return mempty
    
  -- Whether we have a call to the fixpoint variable (Indices.omega),
  -- which is not down a degenerate context branch
  fixpointCall :: Term -> Env.TrackMatches Env.TrackOffset Bool
  fixpointCall (App (Var f) _) 
    | f == Indices.omega = do
      ctx <- Env.liftByOffset rebound_outer_ctx
      liftM not (Eval.degenerateContext ctx)
  fixpointCall _ = 
    return False
  
  -- Replace occurrences of the context applied to the inner
  -- (now uninterpreted) fixpoint variable.
  replace :: Term -> MaybeT (Env.TrackMatches Env.TrackOffset) Term
  replace term = do
    -- Make sure there is only a single recursive call in the term
    Fail.unless (Term.occurrences (Var Indices.omega) term == 1)
    -- and that it contains no bound variables
    Fail.unless (Set.size term_calls == 1)
    
    -- The term we are hoping to replace this term with
    replace_t <- id
      . Env.liftByOffset 
      . Context.apply rebound_outer_ctx
      $ Var Indices.omega
    
    uni <- Unifier.find replace_t term
    
    let mapped_to = concatMap Indices.free (Map.elems uni)
    Fail.when (Indices.containsOmega (Map.keysSet uni))
    Fail.when (Indices.containsOmega mapped_to)
    
    repl_here <- Env.liftByOffset replace_with
    return (Unifier.apply uni repl_here)
    where
    -- Calls to the unrolled fixpoint within the term
    -- we are trying to replace
    term_calls = Term.collect fixpointCall term
    new_fix_var = Var (length new_vars)
    replace_with = app new_fix_var (reverse new_vars)
    
    -- Find calls to the unrolled fixpoint
    fixpointCall :: Term -> Bool
    fixpointCall (App (Var f) _) = f == Indices.omega
    fixpointCall _ = False

  
  -- Remove any recursive calls to the inner fixpoint 
  -- which will be finitely used.
  -- Specifically those for which the simplifier's 'finiteArgFix'
  -- and 'finiteCaseFix' apply.
  expandFiniteCalls :: Index -> Term -> Term -> Term
  expandFiniteCalls fix_var fix = id
    . Env.trackIndices (fix_var, fix)
    . Fold.rewriteM expand
    where
    expand :: Term -> MaybeT (Env.TrackIndices (Index, Term)) Term
    expand (Case (App (Var f) args) alts) = do
      (fix_f, inner_fix) <- Env.tracked
      Fail.unless (f == fix_f)
      Simp.finiteCaseFix (Case (App inner_fix args) alts)
    expand (App (Var f) args) = do
      (fix_f, inner_fix) <- Env.tracked
      Fail.unless (f == fix_f)
      Simp.finiteArgFix (App inner_fix args)
    expand _ = 
      Fail.here
      
    
  
-- | Fixpoint fission.
-- > if E = fission C E'
-- > then C[E] = E'
fission :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) 
  -- | A simplification function to be called within fission 
  -- All indices will have been lifted by one in the supplied term,
  -- and the index of the unrolled fixpoint variable will be 0.
  => (Term -> m Term)
  -> Term 
  -> Context 
  -> m Term
  
fission simplify fix@(Fix fix_info fix_b fix_t) outer_ctx = do

  -- DEBUG
  ctx_s <- showM outer_ctx
  fix_s <- showM fix
  let s1 = "\nSPLITTING:\n" ++ ctx_s ++ "\nFROM:\n" ++ fix_s
  
  simplified_t <- id
    -- DEBUG
   -- . trace s1
  
    -- Simplify the result
    . Env.bind fix_b
    . simplify
    
    -- Add the outer context to every recursive call site
    . Indices.replaceAt 0 (Context.apply (Indices.lift outer_ctx) (Var 0))
    $ fix_t
  
  -- Attempt to strip the context from the term,
  -- this will fail the entire function if it cannot be completed.
  stripped_t <- stripContext simplified_t
  
  let new_term = id
        . Context.apply outer_ctx
        -- We keep the fix info of the original fixpoint, since it will
        -- remain applicable after fission (I think...).
        . Fix fix_info fix_b
        $ stripped_t
        
  -- DEBUG
  new_term_s <- showM new_term
  let s3 = "\nDONE:\n" ++ new_term_s
  id 
  --  . trace s3 
   -- . trace (s1 ++ s3)
    $ return new_term
  
  where
  -- Attempt to remove the @outer_ctx@ from topmost in the given term.
  -- Will first try to float this context to the top so it can be stripped.
  stripContext :: Term -> m Term
  stripContext orig_t = do
      stripped_t <- Context.strip ctx_inside_lam inner_t'
      
      unless (dropped_bs == lam_bs_dropped)
        $ error "Found a case of context stripping being unsound"
         
      return 
        . Term.unflattenLam lam_bs 
        $ stripped_t
    where
    floated_t = id
      . Env.trackIndices dropped_ctx
      . Fold.rewriteM floatCtxUp
      $ orig_t
    
    (dropped_bs, dropped_ctx) = Context.dropLambdas (Indices.lift outer_ctx)
    (lam_bs, inner_t) = Term.flattenLam floated_t
    (lam_bs_dropped, lam_bs_rest) = splitAt (length dropped_bs) lam_bs
    inner_t' = Term.unflattenLam lam_bs_rest inner_t
    
    ctx_inside_lam = Indices.liftMany (length lam_bs) dropped_ctx
    
    floatCtxUp :: Term -> MaybeT (Env.TrackIndices Context) Term
    floatCtxUp cse@(Case t alts) = do
      ctx <- Env.tracked
      alts' <- mapM floatAlt alts
      return
        . Context.apply ctx
        $ Case t alts'
      where
      floatAlt :: Alt -> MaybeT (Env.TrackIndices Context) Alt
      floatAlt alt
        -- If a branch is absurd we can float any context out of it
        | Term.isUnr (get Term.altInner alt) = return alt
      floatAlt (Alt con bs alt_t) = do
        ctx <- Env.trackeds (Indices.liftMany (length bs))
        alt_t' <- Context.strip ctx alt_t
        return (Alt con bs alt_t')
        
    floatCtxUp _ = mzero
  
    
-- | Uses fixpoint fusion to merge a constraint into a fixpoint.
constraintFusion
  :: forall m . (Env.All m, Defs.Read m, Fail.Can m, Memo.Can m)
  => (Term -> m Term)
  -> Constraint 
  -> Term
  -> m Term
constraintFusion simplify
     cons@(Constraint cons_con match_t) 
     term@(App fix@(Fix {}) args) = do
     
  -- Run fixpoint fusion with our special simplification function.
  fusion simplifyAndExpress full_ctx fix
  where
  -- We can use just get here because it's a fixpoint with arguments applied
  result_ty = Type.get term
  
  -- Appending contexts composes them in the natural way
  cons_ctx = Constraint.toContext result_ty cons
  args_ctx = Context.make (\t -> app t args)
  full_ctx = cons_ctx ++ args_ctx

  -- The inner simplification used in constraint fusion
  simplifyAndExpress :: Term -> m Term
  simplifyAndExpress term = do
    -- First we simplify the term
    term' <- simplify term
    let term'' = id
          . Simp.run
          . Env.trackAll (Indices.lift cons_ctx)
          $ Fold.transformM interleaveConstraint term'
    t_s <- showM term
    t_s' <- showM term'
    t_s'' <- showM term''
    
    return
      -- Then we take any pattern matches which unify with the constraint and
      -- express them at the site of any recursive calls to the function
      . Env.trackIndices fix_f
      . Fold.transformM expressPattern
      
      -- We float all potential matches to the constraint as high as they
      -- can go, so they reach the furthest into the term
    --  . Env.trackIndices fix_f
    --  . Fold.rewriteM floatMatchUp
  
      -- At this bit we take the constraint and place it everywhere in the 
      -- term it is applicable, so that the 'expressPattern' step afterwards
      -- has the best chance of finding an expressable constraint pattern.
      . Simp.run
      . Env.trackAll (Indices.lift cons_ctx)
      -- Need to get isoTransforms working properly, no idea what is wrong
      . Fold.transformM interleaveConstraint
  --    . trace ("[constraint fusion]\nbefore:\n" ++ t_s' ++ "\nafter:\n" ++ t_s'')
      $ term'
    where    
    fix_f = 0
    ctx = Indices.lift full_ctx
    orig_term = Context.apply ctx (Var fix_f)
    
    floatMatchUp :: Term -> MaybeT (Env.TrackIndices Index) Term
    floatMatchUp orig_t@(Case fix_t@(App (Var g) _) _) = do
      fix_f <- Env.tracked
      Fail.unless (g == fix_f)
      let inner_cses = id
            . Env.trackIndices fix_t
            $ Term.collectM floatable orig_t
      Fail.when (Set.null inner_cses)
      let inner_cse = head (Set.toList inner_cses)
      return 
       -- . trace ("\n\n[constraint fusion] floating:\n" ++ show (caseOf inner_cse) ++ "\nabove:\n" ++ show fix_t ++ "\n\n")
        $ Eval.run (Term.applyCase inner_cse orig_t)
      where
      floatable :: Term -> Env.TrackIndices Term Bool
      floatable (Case inner_t@(App fix@(Fix {}) _) _) = do
        fix_t <- Env.tracked
        let cse_ctx = Constraint.makeContext cons_con inner_t result_ty
            term = Context.apply cse_ctx fix_t
        return
          $ Unifier.exists orig_term term
      floatable _ = 
        return False
    floatMatchUp _ =
      Fail.here
      
    interleaveConstraint :: Term -> Env.TrackAll Context Term
    interleaveConstraint cse_t@(Case (Var x) _) = do
      ctx <- Env.tracked
      if x `Indices.freeWithin` ctx
      then return (Context.apply ctx cse_t)
      else return cse_t
    interleaveConstraint term = return term

    expressPattern :: Term -> Env.TrackIndices Index Term
    expressPattern (Case cse_t alts) 
      -- If we have found a pattern match which unifies with the original
      -- match context then we should express it at recursive calls to the
      -- function
      | Unifier.exists match_t cse_t = do
        fix_f <- Env.tracked
        let alt_t' = id
              . Env.trackAll (Var fix_f, cse_t)
              . Env.liftTrackedMany (length alt_bs)
              $ Fold.transformM express alt_t
            alts' = l_alts ++ (Alt alt_con alt_bs alt_t' : r_alts)
        return (Case cse_t alts')
      where
      con_n = get Type.constructorIndex cons_con
      (l_alts, Alt alt_con alt_bs alt_t : r_alts) = splitAt (enum con_n) alts
      
      express :: Term -> Env.TrackAll (Term, Term) Term
      express term@(App (Var f) _) = do
        (Var fix_f, cse_t) <- Env.tracked
        let cse_ctx = Constraint.makeContext cons_con cse_t result_ty
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

constraintFusion _ _ _ = 
  Fail.here

