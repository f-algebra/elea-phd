-- | Some term transformation steps that rely on fixpoint fusion.
module Elea.Fusion.Steps
(
  steps, run
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Index hiding ( lift )
import Elea.Fusion.Core
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid


steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = [ varEqApply, caseFixFusion, removeIdFix, fusion, floatConstructors ]

run :: Env.Readable m => Term -> m Term
run = Fold.rewriteStepsM ({-map typeCheck $ -} simpleSteps ++ steps)

simpleSteps :: Env.Readable m => [Term -> m (Maybe Term)]
simpleSteps = map (return .) (Simp.steps ++ Float.steps)

checkedSimple :: Env.Readable m => Term -> m Term
checkedSimple = Fold.rewriteStepsM (map Typing.checkStep simpleSteps)

varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing

removeIdFix :: Env.Readable m => Term -> m (Maybe Term)
removeIdFix fix_t@(Fix (Bind _ fix_ty) _) 
  | ([arg_b@(Bind _ arg_ty)], res_ty) <- flattenPi fix_ty
  , Indices.lift arg_ty == res_ty = do
    let ctx = Context.make fix_ty fix_ty
          $ \_ -> Lam arg_b (Var 0)
    Fail.toMaybe (split checkedSimple fix_t ctx)
removeIdFix _ = 
  return Nothing

floatConstructors :: forall m . Env.Readable m => Term -> m (Maybe Term)
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
      . firstM 
      . map (Fail.toMaybe . split checkedSimple fix_t)
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
  
caseFixFusion :: Env.Readable m => Term -> m (Maybe Term)
caseFixFusion 
    cse_t@(Case (flattenApp -> fix_t@(Fix {}) : i_args) ind_ty alts) = do
  full_ty <- Err.noneM (Typing.typeOf cse_t)
  fix_ty <- Err.noneM (Typing.typeOf fix_t)
  let ctx = Context.make fix_ty full_ty
        $ \t -> Case (unflattenApp (t:i_args)) ind_ty alts
  Fail.toMaybe (fuse checkedSimple ctx fix_t)
caseFixFusion _ = 
  return Nothing
  
fusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fusion full_t@(flattenApp -> outer_f@(Fix outer_b _) : args@(_:_)) = do
  full_ty <- Err.noneM (Typing.typeOf full_t)
  if not (isInd full_ty)
  then return Nothing
  else firstM $ map ($ full_ty) [fixfix, repeatedArg]
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
