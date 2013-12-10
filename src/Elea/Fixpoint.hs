module Elea.Fixpoint
(
  fusion, fission
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Simplifier as Simp
import qualified Elea.Context as Context
import qualified Elea.Foldable as Fold


fusion :: Env.Readable m =>
  (Index -> Context -> Term -> m Term) ->
  Context -> Term -> MaybeT m Term
fusion simplify outer_ctx inner_fix@(Fix fix_b fix_t) = do

  -- Make the type of the new fixpoint we are creating
  result_ty <- Typing.typeOf orig_t
  free_var_bs <- mapM Env.boundAt free_vars
  let free_var_tys = map (get Type.boundType free_var_bs)
      new_fix_ty = Type.unflatten (free_var_tys ++ [result_ty])
      new_lbl = "[" ++ get Type.boundLabel fix_b ++ "]"
      new_fix_b = Bind new_lbl new_fix_ty
  
  simplified_t <- id
    . Env.bindMany (fix_b:free_var_bs)
    -- Finally, run the given simplification function on it.
    . simplify 0 ctx_here
    -- Replace all free variables with new locally bound ones
    . rebindVars
    -- Apply the context to the unwrapped function body
    $ Context.apply ctx_here fix_t
  
  let replaced_t = id 
        . Env.trackOffset
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Fold.rewriteM replace
        -- We replace all occurrences of the old fix variable with omega
        . Env.replaceAt 0 (Var Indices.omega)
        $ simplified_t
  
  -- Fusion has failed if any occurrences of the old fix variable remain
  guard (not (Indices.containsOmega replaced_t))
 
  -- Here's our shiny new fixpoint
  let new_fix = app (Fix new_fix_b) replaced_t
  
  -- Since we used every free variable as a potential argument we'll need
  -- to remove the ones that never change
  Simp.removeConstArgs new_fix
  
  where
  orig_t = Context.apply outer_ctx inner_fix
  free_vars = Indices.free orig_t
  ctx_here = Indices.lift outer_ctx
  new_vars = reverse (map (Var . enum) [1..length free_vars])
  
  rebindVars :: Term -> Term
  rebindVars = 
    concatEndos (zipWith Indices.replaceAt free_vars new_vars)
  
  -- Replace occurrences of the context applied to the inner
  -- (now uninterpreted) fixpoint variable.
  replace :: Term -> MaybeT Env.TrackOffset Term
  replace term 
    | Indices.containsOmega term = do
      -- The term we are replacing
      replace_t <- id
        . Env.liftHere 
        . Context.apply ctx_here 
        $ Var Indices.omega

      uni <- Unifier.find replace_t term
      guard (not (Indices.omega `Map.member` uni))
      
      liftM Unifier.apply uni
        . Env.liftHere 
        $ app (Var 0) new_vars
  replace _ = mzero    
    
  
fission :: Env.Readable m =>
  (Index -> Context -> Term -> m Term)
  Term -> Context -> m Term
fission = undefined
