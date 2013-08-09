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
import qualified Elea.Floating as Float
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

-- TODO use Indices.omega rather than random offsets we remove?
-- TODO only pull in the variables that you need as arguments, not all of them

fuse :: forall m . (Env.Readable m, Fail.Monad m) => 
  (Term -> m Term) -> (Index -> Term -> m Term) -> Context -> Term -> m Term
fuse simplify extract outer_ctx inner_fix@(Fix fix_info fix_b fix_t) = 
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

  simplified_t <- fix_t
    |> Context.apply (Indices.lift outer_ctx)
    |> simplify
    |> Env.bind fix_b
 --   |> trace s1
    
  extracted_t <- simplified_t
    |> extract 0
    |> Env.bind fix_b
  
  -- I gave up commenting at this point, it works because it does...
  let reverted_t = extracted_t
        |> Term.revertMatchesWhen isInnerFixMatch
        |> Env.trackIndices 0
       
  trn_s <- Env.bind fix_b (showM reverted_t)
  let s2 = "\nTRANS:\n" ++ trn_s
  
  depth <- Env.bindingDepth
  let replaced_t = id
        . Env.trackIndices 0
        . Fold.transformM (replace fix_idx arg_vars)
    --    . trace s2
        $ reverted_t
      
  rep_s <- Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    $ showM replaced_t
  let s3 = "\nREP:\n" ++ rep_s
  
  let 
 
  let fix_body = id
    --    . trace s3
     --   . trace (s1 ++ s2 ++ s3)
        . unflattenLam arg_bs
        . substAt 0 inner_fix
        $ replaced_t
        
  let old_rc = Term.occurrences (Var 0) fix_t
      new_rc = Term.occurrences (Var 0) fix_body
      rem_rc = nonFiniteCalls (Var 0) replaced_t
      
  Fail.unless
    . trace (s1 ++ s2 ++ s3)
    $ rem_rc == 0 || new_rc >= old_rc
  
      {-
  let might_recurse = Fold.any (Term.fragmentedUnifierExists fused_t) fix_body
  
  fbs <- Env.bind new_fix_b $ showM fix_body
  
  if not might_recurse && not (new_rec_call_count >= rec_call_count)
  then trace ("FAIL ON:\n" ++ fbs) Fail.here
  else return ()
      -}
 -- Fail.unless (replaced_enough || not inner_f_remained)
  
  -- If this holds then fusion might repeat within itself
  -- Fail.when might_recurse

  done <- unflattenApp (Fix fix_info' new_fix_b fix_body : arg_vars)
   -- You have to do this bit first, otherwise unfoldFixInj doesn't
   -- treat the patterns as matched patterns (since they now feature
   -- lambda bound variables).
    |> Float.removeConstArgs
   -- |> simplify
    |> Float.run
    
  done_s <- showM done
  let s4 = {- s1 ++ -} "\nDONE:\n" ++ done_s
  
  -- This doesn't block anything, and slows things down noticeably.
  -- Fail.when (leftmost done == inner_fix)
  
  id 
    . trace s4 
    $ return done
  where 
  fused_t = Context.apply outer_ctx inner_fix
  fix_info' = set normalForm False fix_info
    
  isInnerFixMatch :: Term -> Env.TrackIndices Index Bool
  isInnerFixMatch cse_t = asks (`Set.member` Indices.free cse_t)
  
  nonFiniteCalls :: Term -> Term -> Int
  nonFiniteCalls func term = term
    |> Fold.transformM removeFiniteCalls
    |> Env.trackIndices func
    |> Term.occurrences func
    where
    removeFiniteCalls :: Term -> Env.TrackIndices Term Term
    removeFiniteCalls term@(Case cse_t ind_ty alts) = do
      func <- ask
      if leftmost cse_t == func 
        && Term.isFinitelyUsed ind_ty alts
      then return (Case (Var Indices.omega) ind_ty alts)
      else return term
    removeFiniteCalls term@(flattenApp -> term_f : args@(_:_)) = do
      func <- ask
      if term_f == func 
        && Term.isFinite (last args)
      then return (Var Indices.omega)
      else return term
    removeFiniteCalls term =
      return term

  replace :: Index -> [Term] -> Term -> Env.TrackIndices Index Term
  replace fix_idx arg_vars term = do
    old_f <- ask
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
        . unflattenApp
        . (Var fix_idx :)
        $ map Indices.lift arg_vars
      where
      liftHere :: Indexed t => t -> t
      liftHere = Indices.liftMany (fromEnum old_f)
      
      replace_t = id
        . liftHere
        . Context.apply (Indices.lift outer_ctx) 
        $ Var 0   

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
      ctx <- ask
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
          ctx <- asks (Indices.liftMany (length bs))
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
    |> Term.revertMatchesWhen isInnerMatch
    |> Env.trackIndices inner_t
  
  isInnerMatch :: Term -> Env.TrackIndices Term Bool
  isInnerMatch (Var x) = asks (Set.member x . Indices.free)
  isInnerMatch _ = return False
  
  buildBranch :: Type -> Nat -> m Term 
  buildBranch ind_ty inj_n = top_t
    |> transform
    |> Env.equals inner_t (Term.altPattern ind_ty inj_n)

