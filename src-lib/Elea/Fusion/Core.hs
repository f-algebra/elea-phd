-- | Performs fixpoint fusion.
-- Don't read the code in here please, it's horrible, hacky and uncommented.
module Elea.Fusion.Core
(
  fuse, split, invent
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Unifier ( Unifier )
import Elea.Index hiding ( lift )
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Floating as Float
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

-- TODO use Indices.omega rather than random offsets we remove?
-- TODO only pull in the variables that you need as arguments, not all of them

fuse :: forall m . (Env.Readable m, Fail.Monad m) => 
  (Term -> m Term) -> 
  (Index -> Context -> Term -> m Term) -> 
  Context -> Term -> m Term
fuse simplify extract outer_ctx inner_fix@(Fix fix_info fix_b fix_t) =   
    -- This is important, having facts remain in a fusion step
    -- is unsound.
    Env.forgetFacts $ do
  ctx_s <- showM outer_ctx
  t_s <- showM inner_fix
  let s1 = "FUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s
  
  next_free_index <- liftM toEnum Env.bindingDepth
  let arg_indices = reverse [0..next_free_index - 1]
      arg_vars = map Var arg_indices
      fix_idx = next_free_index + 1
      new_label = 
        liftM (\t -> "[" ++ t ++ "]")
        $ get boundLabel fix_b

  -- The return type of our new function is the type of the term
  -- we are fusing (fused_t == inner_f inside outer_ctx)
  result_ty <- Err.noneM (Typing.typeOf fused_t)
  -- The arguments to our new function are any free variables in the 
  -- term we are fusing
  var_bs <- mapM Env.boundAt arg_indices
  -- The type of our new function, and its new type binding
  let offsets = reverse [1..length var_bs]
      arg_bs = zipWith Indices.lowerMany offsets var_bs
      new_fix_ty = unflattenPi arg_bs result_ty
      new_fix_b = Bind new_label new_fix_ty
      ctx_here = Indices.lift outer_ctx
      
  (args_ctx, main_ctx) <- id
     . Env.bind fix_b
     $ Term.isolateArguments ctx_here
 
  let applied_t = id
     --   . trace (show outer_ctx ++ " ==> " ++ show args_ctx ++ " with " ++ show main_ctx)
        . Env.trackIndices main_ctx
        . Fold.isoTransformM Term.branchesOnly
            (\t -> Env.trackeds (\ctx -> Context.apply ctx t))
        . Simp.run
        . Context.apply args_ctx
        $ fix_t

 -- let applied_t = Context.apply ctx_here fix_t

  simplified_t <- id
    . trace s1
    . Env.bind fix_b
    . Env.fixpointHere 
    . simplify
    $ applied_t
    
  extracted_t <- id
    . Env.bind fix_b
    . Env.fixpointHere
    . extract 0 ctx_here 
    $ simplified_t
  
  let reverted_t = id
        . Env.trackIndices 0
        . Term.revertMatchesWhenM isInnerFixMatch
        $ extracted_t
       
  simp_s <- Env.bind fix_b (showM simplified_t)
  revt_t <- Env.bind fix_b (showM reverted_t)
  let s2 = "\nSIMPLIFIED:\n" ++ simp_s ++ "\n\nEXTRACTED:\n" ++ revt_t
  
  depth <- Env.bindingDepth
  let replaced_t = id
        . Env.trackIndices 0
        . Fold.transformM (replace fix_idx arg_vars)
        . trace s2
        $ reverted_t
        
  expanded_t <- id
    . Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    . Env.alsoTrack (0, Indices.lift inner_fix)
    . Fold.isoRewriteM Term.restricted (runMaybeT . expandFiniteCalls)
    $ replaced_t
  
  rep_s <- Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    $ showM expanded_t
  let s3 = "\nEXPANDED:\n" ++ rep_s
 
  let fix_body = id
        . trace s3
       --  . trace (s1 ++ s2 ++ s3)
        . unflattenLam arg_bs
        . substAt 0 inner_fix
        $ expanded_t
        
  let old_rc = id
        . Set.size
        . functionCalls (Var 0) 
        $ expanded_t
      new_rc = id
        . Set.size
        . functionCalls (Var 0) 
        $ fix_body
      old_rc' = Term.occurrences (Var 0) fix_t
      new_rc' = Term.occurrences (Var 0) fix_body
      rem_rc = Term.occurrences (Var 0) expanded_t
      
  Fail.unless
  --  . trace s1
  --  . trace (s1 ++ s2 ++ s3)
    $ rem_rc == 0 || new_rc' >= old_rc'

  done <- App (Fix fix_info' new_fix_b fix_body) arg_vars
   -- You have to do this bit first, otherwise unfoldFixInj doesn't
   -- treat the patterns as matched patterns (since they now feature
   -- lambda bound variables).
    |> Float.removeConstArgs
    -- Running 'simplify' here is not in the spirit of the function
   -- |> simplify
    |> Float.run
    
  done_s <- showM done
  let s4 = {- s1 ++ -} "\nDONE:\n" ++ done_s
  
  id 
    . trace s4 
    $ return done
  where 
  fused_t = Context.apply outer_ctx inner_fix
  fix_info' = set normalForm False fix_info
    
  isInnerFixMatch :: Term -> Env.TrackIndices Index Bool
  isInnerFixMatch cse_t = Env.trackeds (`Set.member` Indices.free cse_t)
  
  functionCalls :: Term -> Term -> Set Term
  functionCalls func = id
    . Env.trackIndices func
    . Fold.isoFoldM Term.restricted call
    where
    call :: Term -> Env.TrackIndices Term (Set Term)
    call term@(App func' _) = do
      func <- Env.tracked
      if func' /= func
      then return mempty
      else do
        let min = minimum (Indices.free term)
        return (Set.singleton (Indices.lowerMany (fromEnum min) term))
    call _ = return mempty
  
  expandFiniteCalls :: Term -> MaybeT (Env.AlsoTrack (Index, Term) m) Term
  expandFiniteCalls from@(Case (App (Var f) args) ind_ty alts) = do
    (fix_f, inner_fix) <- Env.tracked
    guard (f == fix_f)
    let cse = Case (App inner_fix args) ind_ty alts
        mby_cse' = Float.unfoldCaseFix cse
    if isJust mby_cse'
    then MaybeT (return mby_cse')
    else MaybeT
       . lift 
       . Float.unsafeUnfoldCaseFix
       $ cse
  expandFiniteCalls term@(App (Var f) args) = do
    (fix_f, inner_fix) <- Env.tracked
    guard (f == fix_f)
    guard (any Term.isFinite args)
    return (App inner_fix args)
  expandFiniteCalls _ = mzero
  
  replace :: Index -> [Term] -> Term -> Env.TrackIndices Index Term
  replace fix_idx arg_vars term = do
    old_f <- Env.tracked
    return (fromMaybe term (tryReplace old_f))
    where
    tryReplace :: Index -> Maybe Term
    tryReplace old_f = do
      guard (old_f `Set.member` Indices.free term)
      
      uni <- Unifier.find replace_t term
      guard (not (old_f `Map.member` uni))
       
      let mapped_to = concatMap Indices.free (Map.elems uni)
      guard (not (liftHere fix_idx `Set.member` mapped_to))
      
      return
        . Unifier.apply uni 
        . liftHere
        . app (Var fix_idx)
        $ map Indices.lift arg_vars
      where
      liftHere :: Indexed t => t -> t
      liftHere = Indices.liftMany (fromEnum old_f)
      
      replace_t = id
        . liftHere
        . Context.apply (Indices.lift outer_ctx) 
        $ Var 0   
        
fuse _ _ _ other = error (show other)

split :: forall m . (Fail.Monad m, Env.Readable m) => 
  (Term -> m Term) -> Term -> Context -> m Term
split transform (Fix fix_info fix_b fix_t) (Indices.lift -> outer_ctx) = do
  full_t_s <- showM (Fix fix_info fix_b fix_t)
  ctx_s <- Env.bind fix_b $ showM outer_ctx
  let s1 = "\n\nSPITTING\n" ++ ctx_s ++ "\n\nFROM\n\n" ++ full_t_s
  unfloated <- id 
    . Env.bind fix_b 
    . transform 
  --  . trace s1 
    . Indices.replaceAt 0 (Context.apply outer_ctx (Var 0))
    $ fix_t
  unf_s <- Env.bind fix_b $ showM unfloated
  let s2 = "\n\nUNFLOATED\n" ++ unf_s
     
 -- flt_s <- Env.bind fix_b (showM floated)
 -- let s3 =  "\n\nFLOATED\n" ++ flt_s
  stripped <- id 
   -- . trace s2
    $ stripContext unfloated
   
  let output = id
       . Context.apply (Indices.lower outer_ctx)
       . Fix fix_info fix_b
       $ stripped
       
  o_s <- showM output
  let s3 = s1 ++ "\n\nGIVES\n" ++ o_s
  return   
    . trace s3
    $ output
  where
  (arg_bs, _) = flattenPi (get boundType fix_b)
                 
  stripContext :: Term -> m Term
  stripContext strip_from = do
    mby_stripped <- Fail.toMaybe (Context.strip outer_ctx strip_from)
    case mby_stripped of
      Just t -> return t
      Nothing -> id
        . liftM (unflattenLam lam_bs)
        . Context.strip ctx_inside_lam 
        $ inner_rhs
    where
    dropped_ctx = Context.dropLambdas outer_ctx
    (lam_bs, inner_rhs) = flattenLam floated
    ctx_inside_lam = Indices.liftMany (length lam_bs) dropped_ctx
    
    floated = id
      . Env.trackIndices dropped_ctx
      . Fold.rewriteM (runMaybeT . floatCtxUp)
      $ strip_from
    
    floatCtxUp :: Term -> MaybeT (Env.TrackIndices Context) Term
    floatCtxUp cse@(Case t ty alts) = do
      ctx <- Env.tracked
      alts' <- mapM floatAlt alts
      return 
        . Context.apply ctx 
        $ Case t ty alts'
      where
      floatAlt :: Alt -> MaybeT (Env.TrackIndices Context) Alt
      floatAlt (Alt bs inner) 
        -- If a branch is absurd we can float any context out of it.
        | isAbsurd inner = return (Alt bs inner)
        | otherwise = do
          ctx <- Env.trackeds (Indices.liftMany (length bs))
          inner' <- Context.strip ctx inner
          return (Alt bs inner')
    floatCtxUp _ = mzero

    
invent :: forall m . (Fail.Monad m, Env.Readable m) => 
  (Term -> m Term) -> Term -> Term -> m Term
invent transform inner_t top_t = do
  Fail.when (inner_t `Term.contains` reverted_top)
  ind_ty <- Err.noneM (Typing.typeOf inner_t)
  Fail.unless (isInd ind_ty && not (Term.isRecursiveInd ind_ty))
  inner_s <- showM inner_t
  top_s <- showM top_t
 --   . trace ("\n\nEXTRACTING:\n" ++ inner_s ++ "\n\nFROM:\n" ++ top_s)
  Term.buildCaseOfM inner_t ind_ty (buildBranch ind_ty)
  where
  reverted_top = top_t
    |> Term.revertMatchesWhenM isInnerMatch
    |> Env.trackIndices inner_t
  
  isInnerMatch :: Term -> Env.TrackIndices Term Bool
  isInnerMatch (Var x) = Env.trackeds (Set.member x . Indices.free)
  isInnerMatch _ = return False
  
  buildBranch :: Type -> Nat -> m Term 
  buildBranch ind_ty inj_n = top_t
    |> transform
    |> Env.equals inner_t (Term.altPattern ind_ty inj_n)
