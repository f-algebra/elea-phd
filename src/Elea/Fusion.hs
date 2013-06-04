-- | Some term transformation steps that rely on fixpoint fusion.
module Elea.Fusion
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
steps = [ fusion
        , floatConstructors 
        , removeIdFix
        ]
        
run :: forall m . Env.Readable m => Term -> m Term
run term = do
  term' <- Float.run term
  mby_fused <- firstM (map (applyStep term') steps)
  case mby_fused of 
    Nothing -> return term'
    Just fused -> do
      ts <- showM term
      ts' <- showM term'
      ts'' <- showM fused
      id
      -- . trace ("\n\nUNFLOATED:\n\n" ++ ts ++ "\n\nUNFUSED:\n\n" ++ ts' ++ "\n\nFINISHED\n\n" ++ ts'') 
        $ run fused
  where
  applyStep :: Term -> (Term -> m (Maybe Term)) -> m (Maybe Term)
  applyStep t = ($ t) . Fold.rewriteOnceM . Typing.checkStep

simpleSteps :: Env.Readable m => [Term -> m (Maybe Term)]
simpleSteps = Simp.stepsM ++ Float.steps

simpleAndFloat :: Env.Readable m => Term -> m Term
simpleAndFloat = Fold.rewriteStepsM (simpleSteps ++ [floatConstructors])

checkedSimple :: Env.Readable m => Term -> m Term
checkedSimple = Fold.rewriteStepsM ({- map Typing.checkStep -} simpleSteps)

varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing

removeIdFix :: Env.Readable m => Term -> m (Maybe Term)
removeIdFix fix_t@(Fix _ (Bind _ fix_ty) _) 
  | ([arg_b@(Bind _ arg_ty)], res_ty) <- flattenPi fix_ty
  , Indices.lift arg_ty == res_ty = do
    let ctx = Context.make fix_ty fix_ty
          $ \_ -> Lam arg_b (Var 0)
    Fail.toMaybe (split checkedSimple fix_t ctx)
removeIdFix _ = 
  return Nothing

floatConstructors :: forall m . Env.Readable m => Term -> m (Maybe Term)
floatConstructors term@(Fix _ fix_b fix_t) 
  | Set.size suggestions == 0 = return Nothing
  | not floatable = return Nothing
  | otherwise = id
      . firstM 
      . map (Fail.toMaybe . split simpleAndFloat term)
      $ Set.toList suggestions
  where
  fix_ty = get boundType fix_b
  (arg_bs, return_ty) = flattenPi fix_ty
  
  floatable :: Bool
  floatable = Set.size (returnCons fix_t) == 1
    where
    returnCons :: Term -> Set Nat
    returnCons (Lam _ t) = returnCons t
    returnCons (Case _ _ alts) = 
      Set.unions (map (returnCons . get altInner) alts)
    returnCons (leftmost -> Inj n _) = Set.singleton n
    returnCons _ = mempty

  suggestions :: Set Context
  suggestions = runReader (suggest fix_t) 1
    where
    suggest :: Term -> Reader Index (Set Context)
    suggest (Lam _ t) = 
      local Indices.lift (suggest t)
    suggest (Case _ ind_ty alts) =
      concatMapM suggestAlt checked_alts
      where
      base_alts = id
        . map (alts !!)
        . filter (isBaseCase ind_ty . toEnum)
        $ [0..length alts - 1]
        
      checked_alts
        | all (isAbsurd . get altInner) base_alts = alts
        | otherwise = base_alts
        
      suggestAlt :: Alt -> Reader Index (Set Context)
      suggestAlt (Alt bs alt_t) =
        local (liftMany (length bs)) (suggest alt_t)
        
    suggest inj_t@(flattenApp -> (Inj inj_n ind_ty : args)) = do
      free_limit <- ask
      let idx_offset = fromEnum free_limit - length arg_bs
      if any (< free_limit) (Indices.free inj_t)
      then return mempty
      else return
         . Set.insert (constContext idx_offset)
         . Set.unions
         $ map (gapContext idx_offset) [0..length args - 1]
      where
      constContext idx_offset = id
        . Context.make fix_ty fix_ty
        . const
        . unflattenLam arg_bs
        . Indices.lowerMany idx_offset
        $ inj_t
        
      gapContext idx_offset gap_n 
        | arg_ty /= ind_ty = mempty
        | otherwise = Set.singleton (Context.make fix_ty fix_ty mkContext)
        where
        arg_ty = id
          . Typing.nthArgument gap_n
          . get boundType
          . (!! fromEnum inj_n)
          $ Typing.unfoldInd ind_ty
          
        (left, _:right) = splitAt (fromEnum gap_n) args
                
        mkContext gap_f = id
          . unflattenLam arg_bs
          . unflattenApp
          $ left' ++ [gap] ++ right'
          where
          left' = id
            . map (Indices.lowerMany idx_offset) 
            $ [Inj inj_n ind_ty] ++ left
          right' = map (Indices.lowerMany idx_offset) right
          
          gap = id
            . unflattenApp 
            . (gap_f :)
            . map (Var . toEnum) 
            $ reverse [0..length arg_bs - 1]
    suggest _ = 
      return mempty
floatConstructors _ = 
  return mzero
  
fusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fusion full_t@(flattenApp -> 
    outer_f@(Fix outer_info outer_b _) : outer_args@(_:_)) = do
  full_ty <- Err.noneM (Typing.typeOf full_t)
  if not (isInd full_ty)
  then return Nothing
  else firstM $ map ($ full_ty) [ fixfix, repeatedArg, fixFact]
  where
  fixfix :: Type -> m (Maybe Term)
  fixfix full_ty
    | isFix (leftmost (head outer_args)) = do
      inner_ty <- Err.noneM (Typing.typeOf inner_f)
      let outer_ctx = id
            . Context.make inner_ty full_ty
            $ \t -> unflattenApp 
              $ outer_f : unflattenApp (t : inner_args) : tail outer_args
      runMaybeT (runFusion outer_ctx)
    where
    inner_f@(Fix {}) : inner_args = flattenApp (head outer_args)
    
    runFusion :: Context -> MaybeT m Term 
    runFusion outer_ctx
      | isVar rec_arg = fuse run outer_ctx inner_f
      | otherwise = 
          Typing.generalise (head inner_args) 
            (\t -> fuse run t (Indices.lift inner_f))
            outer_ctx
      where
      rec_arg = head inner_args
  fixfix _ = 
    return Nothing
  
  repeatedArg :: Type -> m (Maybe Term)
  repeatedArg full_ty
    | x@(Var {}) <- head outer_args
    , any (== x) (tail outer_args) = do
      full_s <- showM full_t
      outer_ty <- Err.noneM (Typing.typeOf outer_f)
      let ctx = id
            . Context.make outer_ty full_ty
            $ \t -> unflattenApp (t : outer_args)
      Fail.toMaybe (fuse run ctx outer_f)
  repeatedArg _ =
    return Nothing
    
  fixFact :: Type -> m (Maybe Term)
  fixFact full_ty = do
    matches <- Env.matches
    mby_t <- firstM 
      . map fuseMatch
      $ Map.toList matches
    return mby_t {-
    case mby_t of
      Nothing -> return Nothing
      Just t -> do
        ms <- showM matches
        trace ms (return (Just t)) -}
    where
    fuseMatch :: (Term, Term) -> m (Maybe Term)
    fuseMatch (match_t, flattenApp -> Inj inj_n ind_ty : inj_args)
      | Just inj_n' <- fusedMatch match_t outer_f =
        if inj_n' == inj_n  
        then return Nothing
        -- If we have already fused in a different pattern matched
        -- to the same term, then this is an absurd branch.
        -- Not sure if this ever comes up though.
        else return (Just (App Absurd full_ty))
        
      | isFix (leftmost match_t)
 --     , all isVar match_args
      , relevantFact = do
        outer_ty <- Err.noneM (Typing.typeOf outer_f)
        let ctx = Context.make outer_ty full_ty buildContext
        mby_t <- Fail.toMaybe (fuse run ctx outer_f)
        return $ do
          t <- mby_t
         -- guard (Fold.all (/= outer_f) t)
          return (addFusedMatch (match_t, inj_n) t)
      where
      Fix {} : match_args = flattenApp match_t
      match_vars = Simp.strictVars match_t
      strict_vars = Simp.strictVars full_t
      
      -- A fact is only relevant to this term if there is a shared variable.
      relevantFact = 
        (not . Set.null) (Set.intersection match_vars strict_vars)
        
      buildContext :: Term -> Term
      buildContext gap_t = 
        Case match_t ind_ty alts
        where
        ind_cons = Typing.unfoldInd ind_ty
        alts = map (buildAlt . toEnum) [0..length ind_cons - 1]
        
        buildAlt :: Nat -> Alt
        buildAlt alt_n = Alt alt_bs alt_t
          where
          alt_bs = id
            . fst
            . flattenPi
            . get boundType
            $ ind_cons !! fromEnum alt_n
            
          bs_count = length alt_bs
          
          arg_idxs = map (_varIndex . liftMany bs_count) inj_args
          new_vars = map (Var . toEnum) (reverse [0..bs_count - 1])
          
          alt_t 
            | alt_n /= inj_n = App Absurd full_ty
            | otherwise = id
                . concatEndos (zipWith replaceAt arg_idxs new_vars)
                . liftMany bs_count
                $ unflattenApp (gap_t : outer_args)
                
    fuseMatch _ = 
      return Nothing
  
fusion _ =
  return Nothing

