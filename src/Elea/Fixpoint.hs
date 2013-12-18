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

  -- DEBUG
  ctx_s <- showM outer_ctx
  t_s <- showM inner_fix
  let s1 = "\nFUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s

  -- Make the type of the new fixpoint we are creating
  result_ty <- return Type.empty -- Type.get orig_t
  free_var_bs <- mapM Env.boundAt free_vars
  let free_var_tys = map (get Type.boundType) free_var_bs
      new_fix_ty = Type.unflatten (free_var_tys ++ [result_ty])
      new_lbl = "[" ++ get Type.boundLabel fix_b ++ "]"
      new_fix_b = Bind new_lbl new_fix_ty
  
  let rebound_t = rebindVars (Context.apply ctx_here fix_t)
  rb_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM rebound_t)
  
      
  simplified_t <- id
    -- DEBUG
    . trace (s1 ++ "\nRB:\n" ++ rb_s)
  
    . Env.bindMany (reverse (fix_b:free_var_bs))
    -- Finally, run the given simplification function on it.
    . simplify 0 ctx_here
    -- Replace all free variables with new locally bound ones
    . rebindVars
    -- Apply the context to the unwrapped function body
    $ Context.apply ctx_here fix_t
    
  -- DEBUG
  simp_s <- Env.bindMany (reverse (fix_b:free_var_bs)) (showM simplified_t)
  let s2 = "\nSIMPLIFIED:\n" ++ simp_s
  
  let replaced_t = id 
        -- DEBUG
        . trace s2
  
        . Env.trackOffset
        -- Now replace all occurrences of the outer context 
        -- composed with omega (the old fix variable)
        . Fold.rewriteM replace
        -- We replace all occurrences of the old fix variable with omega
        . Indices.substAt 0 (Var Indices.omega)
        $ simplified_t
        
  -- DEBUG
  rep_s <- Env.bindMany (reverse (new_fix_b:free_var_bs)) (showM replaced_t)
  let s3 = "\nREPLACED:\n" ++ rep_s
  
  -- Fusion has failed if any occurrences of the old fix variable remain
  Fail.when  
    . Indices.containsOmega
    
    -- DEBUG
    . trace s3
    
    $ replaced_t
 
  -- Here's our shiny new term
  new_term <- id
    -- Since we used every free variable as a potential argument we'll need
    -- to remove the ones that never change
    . Simp.removeConstArgs
    . app (Fix new_fix_b replaced_t) 
    $ reverse new_vars 
  
  -- DEBUG
  final_s <- showM new_term
  let s4 = "\nDONE:\n" ++ final_s
  trace s4 (return new_term)
  
  where
  orig_t = Context.apply outer_ctx inner_fix
  free_vars = id
    . sort
    . toList 
    $ Indices.free orig_t
    
  ctx_here = Indices.lift outer_ctx
  new_vars = map (Var . enum) [1..length free_vars]
  
  rebindVars :: Term -> Term
  rebindVars = id
    . concatEndos 
    . zipWith Indices.replaceAt (map Indices.lift free_vars)
    $ new_vars
  
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

      uni :: Map Index Term <- Unifier.find replace_t term
      guard (not (Indices.omega `Map.member` uni))
      
      liftM (Unifier.apply uni)
        . Env.liftHere 
        $ app (Var 0) (reverse new_vars)
  replace _ = mzero    
    
  
fission :: Env.Readable m =>
  (Index -> Context -> Term -> m Term)
  -> Term -> Context -> m Term
fission = undefined
