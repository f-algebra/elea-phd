-- | Performs fixpoint fusion.
module Elea.Fusion 
(
  run
)
where

import Prelude ()
import Elea.Prelude hiding ( lift )
import Elea.Index
import Elea.Term ( Term (..) )
import Elea.Context ( Context )
import Elea.Show ( showM )
import qualified Elea.Term as Term
import qualified Elea.Type as Type
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Simplifier as Simp
import qualified Elea.Floating as Float
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

type FusionMonad m = (Type.ReadableEnv m, Term.Facts m)

run :: FusionMonad m => Term -> m Term
run = Fold.rewriteStepsM (map typeCheck $ simpleSteps ++ steps)

simpleSteps :: Monad m => [Term -> m (Maybe Term)]
simpleSteps = map (return .) (Simp.steps ++ Float.steps)

checkedSimple :: FusionMonad m => Term -> m Term
checkedSimple = Fold.rewriteStepsM (map typeCheck simpleSteps)

steps :: FusionMonad m => [Term -> m (Maybe Term)]
steps = [ simpleFusion ]

typeCheck :: Type.ReadableEnv m => 
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
simpleFusion (Term.flattenApp -> outer_f@(Fix outer_b _) : first_arg : args)
  | Fix {} <- (head . Term.flattenApp) first_arg =
    Fail.toMaybe (fuse checkedSimple outer_ctx first_arg)
    where
    first_arg_ty =
        get Type.boundType 
      . head . fst
      . Type.flattenFun
      . get Type.boundType 
      $ outer_b
      
    outer_ctx =
      Context.make first_arg_ty
      $ \t -> Term.unflattenApp (outer_f : t : args)
      
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
  var_bs <- mapM Type.boundAt arg_indices
  -- The type of our new function, and its new type binding
  let new_fix_ty = Type.unflattenFun var_bs result_ty
      new_fix_b = Type.Bind new_label new_fix_ty
  
  -- The inner fix unrolled with 'fix_idx' supplied for the
  -- fix variable, and the transformation function applied,
  -- gives us 'transformed_t'
  transformed_t <- trace s1 $
    Type.bindAt fix_idx fix_b
    . transform
    . Context.apply outer_ctx
    . Term.unflattenApp 
    $ subst (Var fix_idx) fix_t : inner_args
  
  trn_s <- showM transformed_t
  let s2 = "\nTRANS:\n" ++ trn_s
  
  -- I gave up commenting at this point, it works because it does...
  let replaced_t = trace s2 $
          flip runReader (fix_idx, new_fix_idx)
        . Term.ignoreFacts
        . Fold.transformM replace
        $ transformed_t
    
  rep_s <- showM replaced_t
  let s3 = "\nREP:\n" ++ rep_s 
    
  trace s3 $ Fail.when (fix_idx `Set.member` freeIndices replaced_t)
  let arg_bs = zipWith (modify Type.boundType . liftMany) [1..] var_bs
  
  let done =
          (\t -> Term.unflattenApp (t : arg_vars))
        . Fix new_fix_b
        . Term.unflattenLam arg_bs
        $ replaced_t 
  
  done_s <- showM done
  let s4 = "\nDONE:\n" ++ done_s
        
  trace s4 $ return done
  where
  -- Remember that because this reads @(Index, Index)@, and is passed to
  -- 'Fold.transformM' these indices will be incremented correctly as
  -- the rewrite descends into the term.
  replace :: Term -> Term.IgnoreFactsT (Reader (Index, Index)) Term
  replace term = do
    (fix_idx, new_fix_idx) <- ask
    let replace_t = Context.apply outer_ctx 
          $ Term.unflattenApp (Var fix_idx : inner_args)
    mby_uni <- Fail.toMaybe (unifier replace_t term)
    case mby_uni of
      Nothing -> return term
      Just uni 
        | Map.member fix_idx uni -> return term
        | otherwise -> 
              return
            . unify uni 
            . Term.unflattenApp 
            $ Var new_fix_idx : arg_vars
          
  Fix fix_b fix_t : inner_args = Term.flattenApp inner_t
  fused_t = Context.apply outer_ctx inner_t
  largest_free_index = (pred . supremum . freeIndices) fused_t
  arg_indices = reverse [0..largest_free_index]
  arg_vars = map Var arg_indices
  new_fix_idx = succ largest_free_index
  fix_idx = succ new_fix_idx
  new_label = 
    liftM (\t -> "[" ++ t ++ "]")
    $ get Type.boundLabel fix_b
  

-- Split has a built in "float to the top" operation?
split :: Fail.Monad m => (Term -> m Term) -> Term -> Term -> m Term
split = undefined

