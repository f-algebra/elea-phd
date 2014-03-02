-- | The two functions which arise from the formula for fixpoint fusion.
-- If we represent the fusion equation as @C[fix F] = fix G@ then, 
-- 'fusion' takes @C@, @fix F@ and returns @fix G@,
-- 'fission' takes @C@, @fix G@ and returns @fix F@,
module Elea.Fixpoint
(
  fusion, fission
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Simplifier as Simp
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Fixpoint fusion.
-- > if E' = fusion C E
-- > then C[E] = E'
fusion :: forall m . (Fail.Can m, Env.Read m, Defs.Read m) 
  -- | A simplification function to be called during fusion.
  -- All indices will have been lifted by one in the supplied term,
  -- and the index of the unrolled fixpoint variable will be 0.
  => (Term -> m Term)
  -> Context 
  -> Term 
  -> m Term
fusion simplify outer_ctx inner_fix@(Fix fix_info fix_b fix_t) = do
  
  -- DEBUG
  ctx_s <- showM outer_ctx
  t_s <- showM inner_fix
  let s1 = "\nFUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s

  -- Make the type of the new fixpoint we are creating
  result_ty <- Type.get orig_t
  free_var_bs <- mapM Env.boundAt free_vars
  let free_var_tys = map (get Type.boundType) free_var_bs
      new_fix_ty = Type.unflatten (reverse (result_ty:free_var_tys))
      new_lbl = "[" ++ get Type.boundLabel fix_b ++ "]"
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
    
    -- Apply the context to the unwrapped function body
    $ Context.apply (Indices.lift outer_ctx) fix_t
    
  -- DEBUG
  simp_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM simplified_t)
  let s2 = "\nSIMPLIFIED:\n" ++ simp_s
  
  -- Before we can replace we need to revert any pattern matches
  -- over the recursive function call, 
  -- as this will rewrite the term we are replacing
  let reverted_t = id
        -- DEBUG
        . trace s2
        . Env.trackIndices 0
        . Term.revertMatchesWhenM isInnerFixMatch
        $ simplified_t
    
  
  -- The new fix body with every occurrence of the context composed
  -- with the old fix variable replaced with the new fix variable
  let replaced_t = id 
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Env.trackOffset
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
  new_term <- id
    . Simp.run
    . app new_fix 
    . map Var 
    $ reverse free_vars
  
  rep_s <- showM new_term
  let s3 = "\nREPLACED:" ++ rep_s
  
  -- Fusion has failed if we do not recurse as much as the original function,
  -- or all recursive occurrences have been removed.
  -- Seems to be a good heuristic, but this is just evidence based.
  let old_rc = Set.size (functionCalls (Var 0) fix_t)
      new_rc = Set.size (functionCalls (Var 0) new_fix_body)
      rem_rc = Term.occurrences (Var Indices.omega) replaced_t
  
  Fail.unless                                                      
    -- DEBUG
    . trace s3
   -- . trace (s1 ++ s2 ++ s3)
    $ rem_rc == 0 
    || new_rc >= old_rc
    
    -- Couple of extra ad hoc reasons fusion would have succeeded.
    -- Trivially sound and terminating so why not.
    || Term.isAbsurd new_term
    || Term.isSimple new_term
    
  
  -- DEBUG
  final_s <- showM new_term
  let s4 = "\nDONE:\n" ++ final_s
  return (trace s4 new_term)
  
  where
  orig_t = Context.apply outer_ctx inner_fix
  free_vars = id
    . sort
    . toList 
    $ Indices.free orig_t
  new_vars = map (Var . toEnum) [0..length free_vars - 1]
  
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
  
  -- Replace occurrences of the context applied to the inner
  -- (now uninterpreted) fixpoint variable.
  replace :: Term -> MaybeT Env.TrackOffset Term
  replace term 
    | Indices.containsOmega term = do
      -- The term we are replacing
      replace_t <- id
        . Env.liftByOffset 
        . Context.apply outer_ctx'
        $ Var Indices.omega

      uni <- Unifier.find replace_t term
      guard (not (Indices.containsOmega (Map.keysSet uni)))
      
      let mapped_to = concatMap Indices.free (Map.elems uni)
      guard (not (Indices.containsOmega mapped_to))
      
      -- The new function call, lifted to the correct indices
      repl_here <- Env.liftByOffset replace_with
      return (Unifier.apply uni repl_here)
    where
    outer_ctx' = (Indices.lower . rebindVars . Indices.lift) outer_ctx
    new_fix_var = Var (length new_vars)
    replace_with = app new_fix_var (reverse new_vars)
      
  replace _ = mzero    
    
  
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
 --   . trace s1
  
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
--    . trace s3 
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
    floatCtxUp cse@(Case ind t alts) = do
      ctx <- Env.tracked
      alts' <- mapM floatAlt alts
      return
        . Context.apply ctx
        $ Case ind t alts'
      where
      floatAlt :: Alt -> MaybeT (Env.TrackIndices Context) Alt
      floatAlt alt
        -- If a branch is absurd we can float any context out of it
        | Term.isAbsurd (get Term.altInner alt) = return alt
      floatAlt (Alt bs alt_t) = do
        ctx <- Env.trackeds (Indices.liftMany (length bs))
        alt_t' <- Context.strip ctx alt_t
        return (Alt bs alt_t')
        
    floatCtxUp _ = mzero
  
    

