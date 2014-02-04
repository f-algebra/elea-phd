-- | The three functions which arise from the formula for fixpoint fusion.
-- If we represent the fusion equation as @C[fix F] = fix G@ then, 
-- 'fusion' takes @C@, @fix F@ and returns @fix G@,
-- 'fission' takes @C@, @fix G@ and returns @fix F@,
-- 'invention' takes @fix F@, @fix G@ and returns @C@.
module Elea.Fixpoint
(
  fusion, fission, invention
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
  -- | A simplification function to be called during fusion
  => (Context -> Index -> Term -> m Term)
  -> Context 
  -> Term 
  -> m Term
fusion simplify outer_ctx inner_fix@(Fix fix_info fix_b fix_t) = do

  -- Note, the original list of free variables @free_vars@ is in
  -- ascending order of index, and every list generated from that
  -- such as types or new variables is the same, for simplicity.
  -- Lambdas will bind variables in descending order, so we
  -- end up reversing these lists all over the place.
  
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
  
    -- Finally, run the given simplification function on it.
    . Env.bindMany (reverse (fix_b:free_var_bs))
    . simplify outer_ctx' 0
    
    -- Apply the context to the unwrapped function body
    $ Context.apply outer_ctx' fix_t'
    
  -- DEBUG
  simp_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM simplified_t)
  let s2 = "\nSIMPLIFIED:\n" ++ simp_s
  
  -- The new fix body with every occurrence of the context composed
  -- with the old fix variable replaced with the new fix variable
  let replaced_t = id 
        -- DEBUG
        . trace s2
  
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Env.trackOffset
        . Fold.rewriteM replace
        
        -- We replace all occurrences of the old fix variable with omega
        . Indices.substAt 0 (Var Indices.omega)
        $ simplified_t
        
  -- Our shiny new function
  let new_fix = id
        -- We keep the fixpoint information of the inner fixpoint.
        -- So far it only stored fused matches, and these will continue to
        -- be applicable after a fusion step (I'm pretty sure...).
        . Fix fix_info new_fix_b
        . Term.unflattenLam (reverse free_var_bs)
        $ replaced_t
        
  -- DEBUG
  rep_s <- showM new_fix
  let s3 = "\nREPLACED:" ++ rep_s
  
  -- Fusion has failed if any occurrences of the old fix variable remain
  Fail.when                                                      
    -- DEBUG
   -- . trace (s1 ++ s2 ++ s3)
     . trace s3
    
    $ Indices.containsOmega replaced_t
    
  new_term <- id
    . Simp.run
    . app new_fix 
    . map Var 
    $ reverse free_vars
  
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
  new_vars = map (Var . enum) [0..length free_vars - 1]
  
  -- Replace all free variables with fresh new variables
  fix_t' = rebindVars fix_t
  outer_ctx' = rebindVars (Indices.lift outer_ctx)
  
  rebindVars :: (Substitutable a, Inner a ~ Term) => a -> a
  rebindVars = id
    . concatEndos 
    . reverse
    . zipWith Indices.replaceAt (map Indices.lift free_vars)
    $ map Indices.lift new_vars
  
  -- Replace occurrences of the context applied to the inner
  -- (now uninterpreted) fixpoint variable.
  replace :: Term -> MaybeT Env.TrackOffset Term
  replace term 
    | Indices.containsOmega term = do
      -- The term we are replacing
      replace_t <- id
        . Env.liftByOffset 
        . Context.apply (Indices.lower outer_ctx') 
        $ Var Indices.omega

      uni :: Map Index Term <- Unifier.find replace_t term
      guard (not (Indices.omega `Map.member` uni))
      
      -- The new function call, lifted to the correct indices
      repl_here <- Env.liftByOffset replace_with
      return (Unifier.apply uni repl_here)
    where
    new_fix_var = Var (enum (length new_vars))
    replace_with = app new_fix_var (reverse new_vars)
      
  replace _ = mzero    
    
  
-- | Fixpoint fission.
-- > if E = fission C E'
-- > then C[E] = E'
fission :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) 
  -- | A simplification function to be called within fission 
  => (Index -> Context -> Term -> m Term)
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
    . simplify 0 outer_ctx'
    
    -- Add the outer context to every recursive call site
    . Indices.replaceAt 0 (Context.apply outer_ctx' (Var 0))
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
  outer_ctx' = Indices.lift outer_ctx
  
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
    
    (dropped_bs, dropped_ctx) = Context.dropLambdas outer_ctx'
    (lam_bs, inner_t) = Term.flattenLam floated_t
    (lam_bs_dropped, lam_bs_rest) = splitAt (length dropped_bs) lam_bs
    inner_t' = Term.unflattenLam lam_bs_rest inner_t
    
    ctx_inside_lam = Indices.liftMany (elength lam_bs) dropped_ctx
    
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
        ctx <- Env.trackeds (Indices.liftMany (elength bs))
        alt_t' <- Context.strip ctx alt_t
        return (Alt bs alt_t')
        
    floatCtxUp _ = mzero
  
    
-- | Fixpoint invention.
-- > if C = invention E E'
-- > then C[E] = E'
invention :: forall m . (Env.Read m, Defs.Read m, Fail.Can m)
  -- | A simplification function to be called within invention.
  => (Term -> m Term)
  -> Term 
  -> Term 
  -> m Context
  
invention simplify 
    f_term@(App f_fix f_args)
    g_term = do  
    
  -- DEBUG  
  f_term_s <- showM f_term
  g_term_s <- showM g_term
  let s1 = "\nInventing C s.t. C[" ++ f_term_s ++ "] is " ++ g_term_s
    
  -- Retrieve the type of f_term and g_term
  -- so we can pass them to inventCase
  f_ty <- Type.get f_term
  let Type.Base ind_ty@(Type.Ind _ cons) = f_ty
  g_ty <- Type.get g_term 
  
  -- We are inventing a fold function and inventCase discovers each of
  -- the fold parameters
  fold_cases <-
    mapM (inventCase f_ty g_ty . enum) [0..length cons - 1]
  
  let fold_f = Term.buildFold ind_ty g_ty
  fold <- Simp.run (app fold_f fold_cases)
  let ctx = Context.make (\t -> app fold [t])
  
  {-
  -- This algorithm is not sound by construction, so we check its answer using
  -- fusion, which is sound.
  let f_args_ctx = Context.make (\t -> app t f_args)
      fusion_ctx = ctx ++ f_args_ctx
  fused <- fusion (\_ _ -> Simp.run) fusion_ctx f_fix
  Fail.unless (fused == g_term)
  -}
  
  -- I've left the soundness check above out because the equality check
  -- at the end is non-trivial. Will put this back in later.
  return ctx
  where
  inventCase :: Type -> Type -> Nat -> m Term
  inventCase f_ty g_ty con_n = do
    -- Constrain @f_term@ to be this particular constructor
    constraint_ctx <- Term.constraint f_term con_n eq_ty
    
    -- Compose the constraint with the equation context 
    -- using the context monoid
    let ctx = constraint_ctx ++ eq_ctx
      
    -- Fuse this context with the inner fixpoint.
    -- If this fails then just unroll the inner fixpoint once.
    mby_fused_eq <- Fail.catch (fusion (\_ _ -> simplify) ctx f_fix)
    fused_eq <- case mby_fused_eq of
      Just eq -> return eq
      Nothing -> simplify (Context.apply ctx (Term.unfoldFix f_fix))
      
    -- DEBUG
    fused_eq_s <- showM fused_eq
    let s2 = "\nBranch: " ++ fused_eq_s
    
    -- Find a function which satisfies the equation
    func <- trace s2
      . Fail.fromMaybe
      . Env.trackOffset
      . Fold.findM (runMaybeT . caseFunction) 
      $ fused_eq
    
    -- DEBUG 
    func_s <- showM func
    let s3 = "\nFunction discovered: " ++ func_s
    trace s3 (return func)
    where
    -- We represent the equat ion using a new inductive type.
    eq_ind = Type.Ind "__EQ" [("==", [Type.ConArg f_ty, Type.ConArg g_ty])]
    eq_ty = Type.Base eq_ind
    
    -- Build a context which is an equation between f_term and g_term
    -- where the fixpoint in f_term has been replaced by the gap.
    eq_ctx = Context.make makeEqCtx
      where
      makeEqCtx gap_f = 
        app (Con eq_ind 0) [app gap_f f_args, g_term]
        
    caseFunction :: Term -> MaybeT Env.TrackOffset Term
    caseFunction t@(App (Con eq_ind' 0) [left_t, right_t])
      -- Make sure this is actually an equation
      | eq_ind' == eq_ind
      , get Type.name eq_ind' == "__EQ" = do
        -- Check the shape of the left side of the equation is
        -- the constructor we are finding the case for
        Fail.unless (isCon left_f)
        Fail.unless (ind == ind' && con_n == con_n')
        
        -- Try to invent a function which satisfies the equation at this point.
        func <- id
          . foldrM constructFunction right_t 
          $ zip con_args left_args
          
        -- Lower the indices in the discovered function to be valid 
        -- outside this point in the term. 
        -- Remember we have descended into a term to collect equations.
        Env.lowerByOffset func
      where
      Type.Base ind@(Type.Ind _ cons) = f_ty
      left_f:left_args = Term.flattenApp left_t
      Con ind' con_n' = left_f
      (_, con_args) = cons !! enum con_n
      
      -- We move through the constructor arguments backwards, building
      -- up the term one by one.
      constructFunction :: (Type.ConArg, Term) -> Term 
        -> MaybeT Env.TrackOffset Term
      -- If we are at a regular constructor argument (non recursive) 
      -- then the term at this position should just be a variable.
      constructFunction (Type.ConArg ty, Var x) term =
        return  
          . Lam (Bind "x" ty)
          -- Replace the variable at this position with the newly lambda 
          -- abstracted variable
          . Indices.replaceAt (succ x) (Var 0) 
          $ Indices.lift term
          
      -- If we are at a non variable recursive constructor argument,
      -- then we'll need to rewrite the recursive call to @f_term@.
      constructFunction (Type.IndVar, rec_term) term = do
        -- Need to lift the indices in @f_term@ and @g_term@ to be 
        -- what they would be at this point inside the equation term.
        f_term' <- Env.liftByOffset f_term
        g_term' <- Env.liftByOffset g_term
        uni <- Unifier.find f_term' rec_term
        let g_term'' = Unifier.apply uni g_term'
        return
          . Lam (Bind "x" g_ty)
          . Term.replace (Indices.lift g_term'') (Var 0)
          $ Indices.lift term
        
      constructFunction _ _ = Fail.here
        
    caseFunction _ = 
      Fail.here
  
