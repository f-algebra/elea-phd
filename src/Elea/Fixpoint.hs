-- | Contains two functions, 'fusion' and 'fission', which perform
-- fixpoint fusion and fixpoint fission respectively. 
-- Both take a simplification function as a first parameter.
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
import qualified Data.Map as Map


fusion :: forall m . (Fail.Can m, Env.Readable m) =>
  (Index -> Context -> Term -> m Term) ->
  Context -> Term -> m Term
fusion simplify outer_ctx inner_fix@(Fix fix_b fix_t) = do

  -- Note, the original list of free variables 'free_vars' is in
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
-- . trace s1
  
    -- Finally, run the given simplification function on it.
    . Env.bindMany (reverse (fix_b:free_var_bs))
    . simplify 0 outer_ctx'
    
    -- Apply the context to the unwrapped function body
    $ Context.apply outer_ctx' fix_t'
    
  -- DEBUG
  simp_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM simplified_t)
  let s2 = "\nSIMPLIFIED:\n" ++ simp_s
  
  -- The new fix body with every occurrence of the context composed
  -- with the old fix variable replaced with the new fix variable
  let replaced_t = id 
        -- DEBUG
     --   . trace s2
  
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Env.trackOffset
        . Fold.rewriteM replace
        
        -- We replace all occurrences of the old fix variable with omega
        . Indices.substAt 0 (Var Indices.omega)
        $ simplified_t
        
  -- Our shiny new function
  let new_fix = id
        . Fix new_fix_b
        . Term.unflattenLam (reverse free_var_bs)
        $ replaced_t
        
  -- DEBUG
  rep_s <- showM new_fix
  let s3 = "\nREPLACED:" ++ rep_s
  
  -- Fusion has failed if any occurrences of the old fix variable remain
  Fail.when  
    -- DEBUG
    . trace (s1 ++ s2 ++ s3)
    
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
        . Env.liftHere 
        . Context.apply (Indices.lower outer_ctx') 
        $ Var Indices.omega

      uni :: Map Index Term <- Unifier.find replace_t term
      guard (not (Indices.omega `Map.member` uni))
      
      -- The new function call, lifted to the correct indices
      repl_here <- Env.liftHere replace_with
      return (Unifier.apply uni repl_here)
    where
    new_fix_var = Var (enum (length new_vars))
    replace_with = app new_fix_var (reverse new_vars)
      
  replace _ = mzero    
    
  
fission :: forall m . (Env.Readable m, Fail.Can m) =>
  (Index -> Context -> Term -> m Term)
  -> Term -> Context -> m Term
fission simplify fix@(Fix fix_b fix_t) outer_ctx = do

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
        . Fix fix_b
        $ stripped_t
        
  -- DEBUG
  new_term_s <- showM new_term
  let s3 = "\nDONE:\n" ++ new_term_s
  id 
--    . trace s3 
    $ return new_term
  
  where
  outer_ctx' = Indices.lift outer_ctx
  
  -- Attempt to remove the 'outer_ctx' from topmost in the given term.
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
  
