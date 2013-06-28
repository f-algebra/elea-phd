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
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Floating as Float
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

-- REWRITE to use Indices.omega rather than random offsets we remove?

fuse :: forall m . (Env.Readable m, Fail.Monad m) => 
  (Index -> Term -> m Term) -> Context -> Term -> m Term
fuse transform outer_ctx inner_f@(Fix fix_info fix_b fix_t) = 
    Env.forgetMatches $ do
  ctx_s <- showM outer_ctx
  t_s <- showM inner_f
  let s1 = "FUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s

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
  
  transformed_t <- id 
    . Env.bind fix_b
    . transform 0
    . trace s1
    . Context.apply (Indices.lift outer_ctx)
    $ fix_t
  
  trn_s <- Env.bind fix_b (showM transformed_t)
  let s2 = "\nTRANS:\n" ++ trn_s
  
  -- I gave up commenting at this point, it works because it does...
  let reverted_t = id
        . Env.trackIndices 0
        . Fold.transformM revertFixMatches 
        $ transformed_t
        
  depth <- Env.bindingDepth
  let replaced_t = id
        . Env.trackIndices 0
        . Fold.transformM replace
        . trace s2
        $ reverted_t
      
  rep_s <- Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    $ showM replaced_t
  let s3 = "\nREP:\n" ++ rep_s
  
  let inner_f_remained = 0 `Set.member` Indices.free replaced_t
 
  let fix_body = id
        . trace s3
     --   . trace (s1 ++ s2 ++ s3)
        . unflattenLam arg_bs
        . substAt 0 inner_f
        $ replaced_t
        
  let was_replaced = 0 `Set.member` Indices.free fix_body
        
  Fail.unless (was_replaced || not inner_f_remained)
    
  done <- id
    . Float.run
    . (\t -> unflattenApp (t : arg_vars))
    $ Fix fix_info new_fix_b fix_body
     
  done_s <- showM done
  let s4 = "\nDONE:\n" ++ done_s
  
  Fail.when (leftmost done == inner_f)
  
  id 
    . trace s4 
    $ return done
  where {-
  transformBranch :: Term -> Env.AlsoTrack Index m Term
  transformBranch term = do
    -- Collect every recursive call to the inner unrolled function
    -- so that we can generalise them
    fix_idx <- ask
    ts <- showM term
    ty <- Err.noneM (Typing.typeOf term)
    tys <- showM ty
    fix_uses <- trace tys $ return mempty --trace ("MEEP " ++ ts) $ Fold.collectM (fixUsage fix_idx) term
    
    ss <- showM (Set.toList fix_uses)
    lift
      . Typing.generaliseMany (Set.toList fix_uses) 
        (\ixs t -> trace ("GEN: " ++ ss ++ " became " ++ show ixs ++ "\nin\n" ++ ts ++ "\ngiving\n" ++ show t) $ transform (Set.fromList ixs) t) 
      . Context.apply (Indices.lift outer_ctx)
      $ term
    where
    fixUsage :: Index -> Term -> MaybeT (Env.AlsoTrack Index m) Term
    fixUsage outer_idx term@(flattenApp -> Var f : args) = do
      fix_idx <- lift ask
      guard (f == fix_idx)
      ts <- showM term
      tyf <- Err.noneM (Typing.typeOf (Var f))
      tyfs <- showM tyf
      ty <- trace ("MERP: " ++ ts ++ " :: " ++ tyfs) $ Err.noneM (Typing.typeOf term)
      tyss <- showM ty
      trace ("FLEEP : " ++ tyss) $ guard (not (isPi ty))
      -- Lower indices to the level they would be 
      -- at in the 'transformBranch' function
      let fix_offset = fromEnum (fix_idx - outer_idx)
      return (Indices.lowerMany fix_offset term)
    fixUsage _ _ = mzero
  -}
  
  revertFixMatches :: Term -> Env.TrackIndices Index Term
  revertFixMatches term@(Case cse_t ind_ty alts) = do
    fix_f <- ask
    if not (fix_f `Set.member` Indices.free cse_t)
    then return term
    else do
      let alts' = zipWith revertAlt [0..] alts
      return (Case cse_t ind_ty alts')
    where
    revertAlt :: Nat -> Alt -> Alt
    revertAlt n (Alt bs alt_t) = Alt bs alt_t'
      where
      rep_t = Indices.liftMany (length bs) cse_t
      alt_t' = Env.replaceTerm (altPattern ind_ty n) rep_t alt_t
  revertFixMatches other = 
    return other
    
  replace :: Term -> Env.TrackIndices Index Term
  replace term = do
    old_f <- ask
    return (fromMaybe term (tryReplace old_f))
    where
    tryReplace :: Index -> Maybe Term
    tryReplace old_f = do
      guard (old_f `Set.member` Indices.free term)
      
      uni <- Unifier.find replace_t term
      guard (not (old_f `Map.member` uni))
      
      return
        . Unifier.apply uni 
        . liftHere
        . unflattenApp
        . (Var fix_idx :)
        $ map Indices.lift arg_vars
      where
      liftHere = Indices.liftMany (fromEnum old_f)
      replace_t = id
        . liftHere
        . Context.apply (Indices.lift outer_ctx) 
        $ Var 0   
  
  fused_t = Context.apply outer_ctx inner_f
  largest_free_index = (pred . supremum . Indices.free) fused_t
  arg_indices = reverse [0..largest_free_index]
  arg_vars = map Var arg_indices
  fix_idx = largest_free_index + 2
  new_label = 
    liftM (\t -> "[" ++ t ++ "]")
    $ get boundLabel fix_b

split :: forall m . (Fail.Monad m, Env.Readable m) => 
  (Term -> m Term) -> Term -> Context -> m Term
split transform (Fix fix_info fix_b fix_t) (Indices.lift -> outer_ctx) =
    Env.forgetMatches $ do
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
 --   . trace s3
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

invent :: (Fail.Monad m, Env.Readable m) => Term -> Term -> m Term
invent inner_t top_t = do
  Fail.when (inner_t `Env.subterm` top_t)
  ind_ty <- Err.noneM (Typing.typeOf inner_t)
  Fail.unless (isInd ind_ty && not (isRecursiveInd ind_ty))
  inner_s <- showM inner_t
  top_s <- showM top_t
  return
    . trace ("\n\nEXTRACTING:\n" ++ inner_s ++ "\n\nFROM:\n" ++ top_s)
    $ Typing.buildCaseOf inner_t ind_ty (const top_t)
  
  
