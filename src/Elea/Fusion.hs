-- | Performs fixpoint fusion.
module Elea.Fusion 
(
  run
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Index hiding ( lift )
import qualified Elea.Unifier as Unifier
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Floating as Float
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

type FusionMonad m = Env.Readable m

run :: FusionMonad m => Term -> m Term
run = Fold.rewriteStepsM ({-map typeCheck $ -} simpleSteps ++ steps)

simpleSteps :: Env.Readable m => [Term -> m (Maybe Term)]
simpleSteps = map (return .) (Simp.steps ++ Float.steps)

checkedSimple :: FusionMonad m => Term -> m Term
checkedSimple = Fold.rewriteStepsM (map typeCheck simpleSteps)

steps :: FusionMonad m => [Term -> m (Maybe Term)]
steps = [ varEqApply, fusion, floatConstructors ]

typeCheck :: FusionMonad m => 
  (Term -> m (Maybe Term)) -> Term -> m (Maybe Term)
typeCheck f t = do
  mby_t' <- f t
  case mby_t' of
    Nothing -> return Nothing
    Just t' -> do
      ty <- Err.noneM (Typing.typeOf t)
      ty' <- Err.noneM (Typing.typeOf t')
      if ty == ty'
      then return (Just t')
      else do
        t_s <- showM t
        t_s' <- showM t'
        ty_s <- showM ty
        ty_s' <- showM ty'
        error 
          $ "Transformation does not preserve type.\n"
          ++ "Before: " ++ t_s ++ ": [" ++ ty_s ++ "]"
          ++ "\nAfter: " ++ t_s' ++ ": [" ++ ty_s' ++ "]"

varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing

caseFixFusion :: Env.Readable m => Term -> m (Maybe Term)
caseFixFusion 
    cse_t@(Case (flattenApp -> fix_t@(Fix {}) : args) ind_ty alts) = do
  fix_ty <- Err.noneM (Typing.typeOf fix_t)
  cse_ty <- Err.noneM (Typing.typeOf cse_t)
  let outer_ctx = Context.make fix_ty cse_ty
        $ \t -> Case (unflattenApp (t:args)) ind_ty alts
  Fail.toMaybe (fuse checkedSimple outer_ctx fix_t)

fusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fusion full_t@(flattenApp -> outer_f@(Fix outer_b _) : args@(_:_)) = do
  full_ty <- Err.noneM (Typing.typeOf full_t)
  if not (isInd full_ty)
  then return Nothing
  else firstM ($ full_ty) [fixfix, repeatedArg]
  where
  fixfix :: Type -> m (Maybe Term)
  fixfix full_ty
    | isFix (leftmost (head args)) = do
      inner_ty <- Err.noneM (Typing.typeOf inner_f)
      let outer_ctx = id
            . Context.make inner_ty full_ty
            $ \t -> unflattenApp 
              $ outer_f : unflattenApp (t : inner_args) : tail args
      runMaybeT (runFusion outer_ctx)
    where
    inner_f@(Fix {}) : inner_args = flattenApp (head args)
    
    runFusion :: Context -> MaybeT m Term 
    runFusion outer_ctx
      | isVar rec_arg = fuse run outer_ctx inner_f
      | otherwise = 
          Typing.generalise 
            (head inner_args) 
            (flip (fuse run) (Indices.lift inner_f))
            outer_ctx
      where
      rec_arg = head inner_args
  fixfix _ = 
    return Nothing
  
  repeatedArg :: Type -> m (Maybe Term)
  repeatedArg full_ty
    | x@(Var {}) <- head args
    , any (== x) (tail args) = do
      full_s <- showM full_t
      outer_ty <- Err.noneM (Typing.typeOf outer_f)
      let ctx = id
            . Context.make outer_ty full_ty
            $ \t -> unflattenApp (t : args)
      Fail.toMaybe (fuse run ctx outer_f)
  repeatedArg _ =
    return Nothing
fusion _ =
  return Nothing
  
floatConstructors :: forall m . FusionMonad m => Term -> m (Maybe Term)
floatConstructors fix_t@(Fix fix_b _) = runMaybeT $ do
  -- Suggest any constructor contexts, then try then out using 'split'
  depth <- Env.bindingDepth
  suggestions <- lift $ Fold.foldM (suggest depth) fix_t
  fix_t_s <- showM fix_t
  if Set.size suggestions == 0
  then mzero
  else do
    sug_ss <- mapM showM (Set.toList suggestions)
    MaybeT
      . firstM (Fail.toMaybe . split checkedSimple fix_t) 
      . Set.toList 
      $ suggestions
  where
  fix_ty = get boundType fix_b
  (arg_bs, return_ty) = flattenPi fix_ty
  
  -- This value is the number of bindings that will be made 
  -- by the fix and lambda bindings. It allows us to equate the
  -- return type outside the term and the type inside.
  ty_offset = length arg_bs
  
  -- Return any contexts which might be constructors we can float out
  suggest :: Int -> Term -> m (Set Context)
  suggest outer_depth inj_term@(flattenApp -> inj@(Inj {}) : inj_args) = do
    -- idx_offset is how many more indices have been bound 
    -- at this point within the term
    inner_depth <- Env.bindingDepth
    let idx_offset = (inner_depth - outer_depth) - ty_offset
    
    -- This constructor must have the same type as the term we are floating
    let return_ty' = Indices.liftMany idx_offset return_ty
    inner_ty <- Err.noneM (Typing.typeOf inj_term)
    if inner_ty /= return_ty'
    then return mempty
    else do
      gaps <- concatMapM (suggestGap idx_offset) [0..length inj_args - 1]
      let const = suggestConst idx_offset
      return (const `Set.union` gaps)
    where
    -- Returns the constructor as a constant context, if this context
    -- would be valid outside of the term
    suggestConst :: Int -> Set Context
    suggestConst idx_offset =
      if any (< toEnum idx_offset) (Indices.free inj_term)
      then mempty
      else Set.singleton 
         . Context.make fix_ty fix_ty
         . const 
         . unflattenLam arg_bs
         . Indices.lowerMany idx_offset 
         $ inj_term
    
    -- A wrapper around 'suggestGapMaybe' to convert its maybe output
    -- into a single or zero element set. This returns the constructor 
    -- as a context with the gap at argument 'gap_pos', as long as this
    -- would be a valid context outside of the term.
    suggestGap :: Int -> Int -> m (Set Context)
    suggestGap idx_offset gap_pos = id
      . liftM (maybe mempty Set.singleton)
      . runMaybeT 
      $ suggestGapMaybe 
      where
      -- Lift indices in the outer return type to properly match 
      -- the point within the term we are at
      return_ty' = Indices.liftMany idx_offset return_ty
      
      suggestGapMaybe :: MaybeT m Context
      suggestGapMaybe = do
        -- The gap must have the same type as the overall term
        gap_ty <- Err.noneM (Typing.typeOf gap)
        guard (gap_ty == return_ty')
        
        -- A context is not valid if it has any indices which do not
        -- exist outside of the term
        let freeIndices = concatMap Indices.free (left ++ right) 
        guard (all (>= toEnum idx_offset) freeIndices)
        
        return (Context.make fix_ty fix_ty mkContext)
        where
        (left, gap:right) = splitAt gap_pos inj_args
        
        mkContext gap_f = id
          . unflattenLam arg_bs
          . unflattenApp
          $ left' ++ [gap] ++ right'
          where
          left' = map (Indices.lowerMany idx_offset) ([inj] ++ left)
          right' = map (Indices.lowerMany idx_offset) right
          
          gap = id
            . unflattenApp 
            . (gap_f :)
            . map (Var . toEnum) 
            $ reverse [0..length arg_bs - 1]
  suggest _ _ = 
    return mempty
floatConstructors _ = 
  return mzero

-- REWRITE to use Indices.omega rather than random offsets we remove?

fuse :: forall m . (FusionMonad m, Fail.Monad m) => 
  (Term -> m Term) -> Context -> Term -> m Term
fuse transform outer_ctx inner_f@(Fix fix_b fix_t) = do
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
  
  transformed_t <- id -- . trace s1 
    . Env.bind fix_b
    . transform
    . Context.apply (Indices.lift outer_ctx)
    $ fix_t
  
  trn_s <- Env.bind fix_b (showM transformed_t)
  let s2 = "\nTRANS:\n" ++ trn_s
  
  -- I gave up commenting at this point, it works because it does...
  let replaced_t = id -- . trace s2
        . Env.trackIndices 0
        . Fold.transformM replace
        $ transformed_t
  
  rep_s <- Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    $ showM replaced_t
  let s3 = "\nREP:\n" ++ rep_s
  
  id -- . trace s3
    $ Fail.when (0 `Set.member` Indices.free replaced_t)

  let done = id
        . Float.run
        . (\t -> unflattenApp (t : arg_vars))
        . Fix new_fix_b
        . unflattenLam arg_bs
        . Indices.lower
        $ replaced_t
        
  done_s <- showM done
  let s4 = "\nDONE:\n" ++ done_s
  
  id -- . trace s4 
    $ return done
  where
  replace :: Term -> Env.TrackIndices Index Term
  replace term = do
    idx_offset <- ask
    let liftHere = Indices.liftMany (fromEnum idx_offset)
        replace_t = id
          . liftHere
          . Context.apply (Indices.lift outer_ctx) 
          $ Var 0
    mby_uni <- Fail.toMaybe (Unifier.find replace_t term)
    case mby_uni of
      Nothing -> return term
      Just uni 
        | Map.member idx_offset uni -> return term
        | otherwise -> id
            . return
            . Unifier.apply uni 
            . liftHere
            . unflattenApp
            . (Var fix_idx :)
            $ map Indices.lift arg_vars
          
  fused_t = Context.apply outer_ctx inner_f
  largest_free_index = (pred . supremum . Indices.free) fused_t
  arg_indices = reverse [0..largest_free_index]
  arg_vars = map Var arg_indices
  fix_idx = largest_free_index + 2
  new_label = 
    liftM (\t -> "[" ++ t ++ "]")
    $ get boundLabel fix_b

split :: (Fail.Monad m, FusionMonad m) => 
  (Term -> m Term) -> Term -> Context -> m Term
split transform (Fix fix_b fix_t) (Indices.lift -> outer_ctx) = do
  full_t_s <- showM (Fix fix_b fix_t)
  ctx_s <- Env.bind fix_b $ showM outer_ctx
  let s1 = "\n\nSPITTING\n" ++ ctx_s ++ "\n\nFROM\n\n" ++ full_t_s
  unfloated <- id -- . trace s1 
    . Env.bind fix_b  
    . transform 
    . Indices.replaceAt 0 (Context.apply outer_ctx (Var 0))
    $ fix_t
  unf_s <- Env.bind fix_b $ showM unfloated
  let s2 = "\n\nUNFLOATED\n" ++ unf_s
  let floated = id -- . trace s2
        . Env.trackIndices dropped_ctx
        . Fold.rewriteM (runMaybeT . floatCtxUp)
        $ unfloated
      (lam_bs, inner_rhs) = flattenLam floated
      ctx_inside_lam = Indices.liftMany (length lam_bs) dropped_ctx
  flt_s <- Env.bind fix_b (showM floated)
  let s3 =  "\n\nFLOATED\n" ++ flt_s
  stripped_rhs <- -- trace s3 $ 
    Context.strip ctx_inside_lam inner_rhs
  return 
    . Context.apply (Indices.lower outer_ctx)
    . Fix fix_b
    . unflattenLam lam_bs
    $ stripped_rhs
  where
  dropped_ctx = Context.dropLambdas outer_ctx
  (arg_bs, _) = flattenPi (get boundType fix_b)
  
  floatCtxUp :: Term -> MaybeT (Env.TrackIndices Context) Term
  floatCtxUp (Case t ty alts) = do
    ctx <- ask
    alts' <- mapM floatAlt alts
    return 
      . Context.apply ctx 
      $ Case t ty alts'
    where
    floatAlt :: Alt -> MaybeT (Env.TrackIndices Context) Alt
    floatAlt (Alt bs inner) = do
      ctx <- asks (Indices.liftMany (length bs))
      inner' <- Context.strip ctx inner
      return (Alt bs inner')
  floatCtxUp _ = mzero


