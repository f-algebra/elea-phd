-- | Performs fixpoint fusion.
module Elea.Fusion 
(
  run
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
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

type FusionMonad m = Env.Readable m

run :: FusionMonad m => Term -> m Term
run = Fold.rewriteStepsM (map typeCheck $ simpleSteps ++ steps)

simpleSteps :: Monad m => [Term -> m (Maybe Term)]
simpleSteps = map (return .) (Simp.steps ++ Float.steps)

checkedSimple :: FusionMonad m => Term -> m Term
checkedSimple = Fold.rewriteStepsM (map typeCheck simpleSteps)

steps :: FusionMonad m => [Term -> m (Maybe Term)]
steps = [ simpleFusion ]

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

simpleFusion :: FusionMonad m => Term -> m (Maybe Term)
simpleFusion (flattenApp -> outer_f@(Fix outer_b _) : first_arg : args)
  | Fix {} <- (head . flattenApp) first_arg =
    Fail.toMaybe (fuse checkedSimple outer_ctx first_arg)
    where
    first_arg_ty =
        get boundType 
      . head . fst
      . flattenPi
      . get boundType 
      $ outer_b
      
    outer_ctx =
      Context.make first_arg_ty
      $ \t -> unflattenApp (outer_f : t : args)
      
simpleFusion _ = return mzero


fuse :: forall m . (FusionMonad m, Fail.Monad m) => 
  (Term -> m Term) -> Context -> Term -> m Term
fuse transform outer_ctx inner_t = do
  ctx_s <- showM outer_ctx
  t_s <- showM inner_t
  let s1 = "FUSING:\n" ++ ctx_s ++ "\nWITH\n" ++ t_s

  -- The return type of our new function is the type of the term
  -- we are fusing (fused_t == inner_t inside outer_ctx)
  result_ty <- Err.noneM (Typing.typeOf fused_t)
  -- The arguments to our new function are any free variables in the 
  -- term we are fusing
  var_bs <- mapM Env.boundAt arg_indices
  -- The type of our new function, and its new type binding
  let offsets = reverse [1..length var_bs]
      arg_bs = zipWith (modify boundType . Indices.lowerMany) offsets var_bs
      new_fix_ty = unflattenPi arg_bs result_ty
      new_fix_b = Bind new_label new_fix_ty
  
  transformed_t <- trace s1 $
    Env.bind fix_b
    . transform
    . Context.apply (lift outer_ctx)
    . unflattenApp 
    . (fix_t :) 
    $ map lift inner_args
  
  trn_s <- Env.bind fix_b (showM transformed_t)
  let s2 = "\nTRANS:\n" ++ trn_s
  
  -- I gave up commenting at this point, it works because it does...
  let replaced_t = trace s2 $
          flip runReader 0
        . Fold.transformM replace
        $ transformed_t
    
  rep_s <- Env.bindAt 0 fix_b
    . Env.bindAt fix_idx new_fix_b
    $ showM replaced_t
  let s3 = "\nREP:\n" ++ rep_s

  let done =
          (\t -> unflattenApp (t : arg_vars))
        . Fix new_fix_b
        . unflattenLam arg_bs
        . lower
        $ replaced_t
        
  done_s <- showM done
  let s4 = "\nDONE:\n" ++ done_s
  
  trace s3 $ trace s4 $ Fail.when (0 `Set.member` freeIndices replaced_t)
     
  return done
  where
  replace :: Term -> Reader Index Term
  replace term = do
    idx_offset <- ask
    let liftHere :: Liftable a => a -> a
        liftHere = liftMany (fromEnum idx_offset)
        replace_t = 
            liftHere
          . Context.apply (lift outer_ctx)
          . unflattenApp 
          . (Var 0 :)
          $ map lift inner_args
    mby_uni <- Fail.toMaybe (Unifier.find replace_t term)
    case mby_uni of
      Nothing -> return term
      Just uni 
        | Map.member (liftHere 0) uni -> return term
        | otherwise -> 
              return
            . liftAt idx_offset
            . Unifier.apply uni 
            . liftHere
            . unflattenApp 
            . (Var fix_idx :)
            $ map lift arg_vars
          
  Fix fix_b fix_t : inner_args = flattenApp inner_t
  fused_t = Context.apply outer_ctx inner_t
  largest_free_index = (pred . supremum . freeIndices) fused_t
  arg_indices = reverse [0..largest_free_index]
  arg_vars = map Var arg_indices
  fix_idx = largest_free_index + 2
  new_label = 
    liftM (\t -> "[" ++ t ++ "]")
    $ get boundLabel fix_b
  

-- Split has a built in "float to the top" operation?
split :: Fail.Monad m => (Term -> m Term) -> Term -> Term -> m Term
split = undefined

