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
import qualified Elea.Terms as Term
import qualified Elea.Context as Context
import qualified Elea.Typing as Typing
import qualified Elea.Unifier as Unifier
import qualified Elea.Floating as Float
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid


steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = id
  . map Typing.checkStep
  $ [ floatConstructors 
    , fusion
    , removeIdFix
    ]
        
{-# INLINEABLE run #-}
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
      run fused
      --  |> trace ("\n\nUNFUSED:\n\n" ++ ts' ++ "\n\nFINISHED\n\n" ++ ts'') 
  where
  applyStep :: Term -> (Term -> m (Maybe Term)) -> m (Maybe Term)
  applyStep t f = Fold.rewriteOnceM withTrace t
    where
    withTrace t = do
      mby_t <- f t 
      case mby_t of
        Nothing -> return Nothing
        Just t' -> do
          ts <- showM t
          ts' <- showM t'
          Just t'
          --  |> trace ("\n\nTRANSFORMED:\n" ++ ts ++ "\n\nTO:\n" ++ ts')
            |> return

simpleSteps :: Env.Readable m => [Term -> m (Maybe Term)]
simpleSteps = Simp.stepsM ++ Float.steps

simpleAndFloat :: Env.Readable m => Term -> m Term
simpleAndFloat = Fold.rewriteStepsM 
  $ simpleSteps ++ [Fold.rewriteOnceM floatConstructors]

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
        . filter (Term.isBaseCase ind_ty . toEnum)
        $ [0..length alts - 1]
        
      checked_alts
        | all (isAbsurd . get altInner) base_alts = alts
        | otherwise = base_alts
        
      suggestAlt :: Alt -> Reader Index (Set Context)
      suggestAlt (Alt bs alt_t) =
        local (liftMany (length bs)) (suggest alt_t)
        
    suggest (Absurd _) = const (Absurd fix_ty)
      |> Context.make fix_ty fix_ty
      |> Set.singleton
      |> return
      
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
          . Term.nthArgument gap_n
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
  else firstM $ map ($ full_ty) [ fixfix, repeatedArg, fixFact ]
  where
  fixfix :: Type -> m (Maybe Term)
  fixfix full_ty
    | isFix (leftmost (head outer_args)) = do
      inner_ty <- Err.noneM (Typing.typeOf inner_f)
      let outer_ctx = id
            . Context.make inner_ty full_ty
            $ \t -> unflattenApp 
              $ outer_f : unflattenApp (t : inner_args) : tail outer_args
      runMaybeT
        . Env.alsoTrack outer_f
        $ runFusion outer_ctx
    where
    inner_f@(Fix {}) : inner_args = flattenApp (head outer_args)
    
    runFusion :: Context -> Env.AlsoTrack Term (MaybeT m) Term 
    runFusion outer_ctx
      | isVar rec_arg = fuse run extract outer_ctx inner_f
      | otherwise = id
          . Term.generalise (head inner_args) 
            (\_ ctx -> fuse run extract ctx (Indices.lift inner_f))
          $ outer_ctx
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
      Fail.toMaybe (fuse run (const return) ctx outer_f)
  repeatedArg _ =
    return Nothing
    
  fixFact :: Type -> m (Maybe Term)
  fixFact full_ty = do
    matches <- Env.matches
    mby_t <- matches
      |> Map.toList
      |> map fuseMatch
      |> firstM
    return mby_t {-
    case mby_t of
      Nothing -> return Nothing
      Just t -> do
        ms <- showM matches
        trace ms (return (Just t)) -}
    where
    fuseMatch :: (Term, (Term, Int)) -> m (Maybe Term)
    fuseMatch (match_t, 
              (inj_t@(flattenApp -> Inj inj_n ind_ty : inj_args), m_depth))
      | Just inj_n' <- fusedMatch match_t outer_f =
        if inj_n' == inj_n  
        then return Nothing
        -- If we have already fused in a different pattern matched
        -- to the same term, then this is an absurd branch.
        -- Not sure if this ever comes up though.
        else return (Just (Absurd full_ty))

      | isFix (leftmost match_t)
 --     , all isVar match_args
      , relevantFact = do
        outer_ty <- Err.noneM (Typing.typeOf outer_f)
        let ctx = Context.make outer_ty full_ty buildContext
        -- We add the new fused matches to the info of our existing 
        -- fixpoint, since this will be carried over to the fixpoint
        -- that fusion produces.
        let outer_f' = outer_f
              |> addFusedMatch (match_t, inj_n)
              |> addFusedMatches (get fusedMatches match_inf)
        mby_t <- Fail.toMaybe (fuse run floatInwards ctx outer_f')
        match_s <- showM match_t
        outer_s <- showM full_t
        inj_s <- showM inj_t
        let msg | isJust mby_t = id
                | otherwise = 
                    trace ("\n\nFailed to merge:\n" ++ match_s ++ "\n == " ++ inj_s ++ "\n\nwith:\n" ++ outer_s)
        msg $ return $ do
          t <- mby_t
          t |> cleanupAbsurdities
            |> return
      where
      Fix match_inf _ _ : match_args = flattenApp match_t
      match_vars = Simp.strictVars match_t
      strict_vars = Simp.strictVars full_t
      
      -- A fact is only relevant to this term if there is a shared variable,
      -- and no variables that have been bound after the match was made.
      relevantFact = not distinct_vars && not bound_later
        where 
        distinct_vars = strict_vars 
          |> Set.intersection match_vars
          |> Set.null
          
        bound_later = strict_vars
          |> concatMap Indices.free
          |> any (< toEnum m_depth)
      
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
            | alt_n /= inj_n = Absurd full_ty
            | otherwise = id
                . concatEndos (zipWith replaceAt arg_idxs new_vars)
                . liftMany bs_count
                $ unflattenApp (gap_t : outer_args)
                
      -- The transformation function that is passed to the Core.fuse
      -- call for fixFact fusion. It is fusion plus 
      -- the 'floatCtxMatchInwards' transformation.
      floatInwards :: forall m . (Env.Readable m, Fail.Monad m) => 
        Index -> Term -> m Term
      floatInwards _ = Fold.rewriteStepsM 
          $ Simp.stepsM ++ Float.steps ++ [floatCtxMatchInwards]
        where
        -- We need the pattern match for the context (viz. over match_t) 
        -- to be as far in as possible in order for fusion to succeed,
        -- viz. that C[f] can be unified at some point. Otherwise we
        -- end up with C[match ... with ... f ...].
        floatCtxMatchInwards :: Term -> m (Maybe Term)
        floatCtxMatchInwards = Float.commuteMatchesWhen when 
          where
          when :: Term -> Term -> m Bool
          when (Case outer_t _ _) (Case inner_t _ _) =
            return
              $ Unifier.exists match_t outer_t
              && not (Unifier.exists match_t inner_t)
              
      cleanupAbsurdities :: Term -> Term
      cleanupAbsurdities = Fold.rewrite cleanup
        where
        cleanup :: Term -> Maybe Term
        cleanup (Case _ _ alts) = do
          (alt_t:alt_ts) <- alts 
            |> mapM loweredAltTerm
            $> sortBy absurdsGreater
          guard (all isAbsurd alt_ts)
          return alt_t
          where
          absurdsGreater (Absurd _) (Absurd _) = EQ
          absurdsGreater (Absurd _) _ = LT
          absurdsGreater _ (Absurd _) = GT
          absurdsGreater _ _ = EQ
          
          loweredAltTerm :: Alt -> Maybe Term
          loweredAltTerm (Alt bs alt_t) = do
            guard (Indices.lowerableBy (length bs) alt_t)
            return (Indices.lowerMany (length bs) alt_t)
        cleanup _ = Nothing

    fuseMatch _ = 
      return Nothing
  
fusion _ =
  return Nothing

extract :: forall m . Env.Readable m =>  
  Index -> Term -> Env.AlsoTrack Term m Term
extract inner_f term = do
  can_extract <- extractable
  if not can_extract
  then return term
  else do
    outer_f <- ask
    lift
      . Env.alsoTrack (outer_f, inner_f) 
      -- We descend into branches as long as the 'inner_f' function
      -- is not pattern matched by that branch.
      . Term.descendWhileM functionNonMatched doExtract 
      $ term
  where
  functionNonMatched :: Term -> Env.AlsoTrack (Term, Index) m Bool
  functionNonMatched (Case cse_t _ _) = do
    (_, inner_f) <- ask
    return (not (inner_f `Set.member` Indices.free cse_t))
  functionNonMatched _ = 
    return False
  
  extractable :: Env.AlsoTrack Term m Bool
  extractable = do
    outer_f <- ask
    ty <- Err.noneM (Typing.typeOf outer_f)
    let (args, ret) = flattenPi ty
    return 
      $ length args == 1
      && isInd ret 
      && not (Term.isRecursiveInd ret)
  
  doExtract :: Term -> Env.AlsoTrack (Term, Index) m Term
  doExtract (flattenApp -> inj@(Inj {}) : args) = do
    args' <- mapM doExtract args
    return (unflattenApp (inj : args'))
  doExtract term = do
    (outer_f, inner_f) <- ask
    inner_calls <- id
      . lift
      . Env.alsoTrack inner_f 
      $ Term.collectM isInnerCall term
    lift
      . Env.alsoTrack outer_f
      . Term.generaliseMany (Set.toList inner_calls) extraction
      $ term
    where
    isInnerCall :: Term -> Env.AlsoTrack Index m Bool
    isInnerCall term@(flattenApp -> Var f : args) = do
      inner_f <- ask
      if inner_f /= f 
      then return False
      else do
        ty <- Err.noneM (Typing.typeOf term)
        return (not (isPi ty))
    isInnerCall _ = 
      return False
      
    extraction :: [Index] -> Term -> Env.AlsoTrack Term m Term
    extraction gen_vars term = do
      outer_f <- ask
      mby_t <- lift 
        . Fail.toMaybe 
        . concatEndosM (map (invent run . App outer_f . Var) gen_vars) 
        $ term
      case mby_t of 
        Nothing -> return term
        Just t -> do
          t' <- lift (run t)
          ts <- showM t'
          return $ {- trace ("GOT: " ++ ts) -} t'
  {-
fixfixSimplifier :: forall m . Env.Readable m =>  
  Int -> Index -> Term -> Env.AlsoTrack Term m Term
fixfixSimplifier inner_arg_count inner_f = id
  . Env.alsoWith (\outer_f -> (outer_f, inner_f))
  . Term.descendWhileM calledWithNonFreeArgs transformBranch
  where
  -- Whether the given index is called as a function using arguments
  -- that contain variables which are not free here. We cannot generalise
  -- an inner function call unless all of the variables it is called with
  -- are free, so we use this to recurse into the branches of the term
  -- until they become free.
  calledWithNonFreeArgs :: Term -> Env.AlsoTrack (Term, Index) m Bool
  calledWithNonFreeArgs term@(Case {}) = do
    (_, inner_f) <- ask
    Fold.anyM (nonFreeArgs inner_f) term
    where
    nonFreeArgs :: Index -> Term -> Env.AlsoTrack (Term, Index) m Bool
    nonFreeArgs inner_f term@(flattenApp -> Var f : args) = do
      (_, inner_f_here) <- ask
      if inner_f_here /= f
      then return False
      else do
        let idx_diff = inner_f_here - inner_f
        return (any (< idx_diff) (Indices.free term))
    nonFreeArgs _ _ = 
      return False
  calledWithNonFreeArgs _ = return False
  
  transformBranch :: Term -> Env.AlsoTrack (Term, Index) m Term
  transformBranch term = do
    -- Collect every recursive call to the inner unrolled function
    -- so that we can generalise them
    fix_uses <- id 
      . Env.alsoWith snd
      $ Term.collectM isInnerCall term
    (outer_f, _) <- ask
    Env.alsoWith (\(outer_f, _) -> outer_f)
      . Term.generaliseMany 
          (Set.toList fix_uses) 
          (\ixs t -> trace ("\nGEN:" ++ show fix_uses ++ "\nWITHIN:\n" ++ show term ++ "\nGIVES:\n" ++ show t) $ simplifyAndExtract (Set.fromList ixs) t) 
      $ term
    where
    isInnerCall :: Term -> Env.AlsoTrack Index m Bool
    isInnerCall term@(flattenApp -> Var f : args) = do
      inner_f <- ask
      return 
         $ inner_f == f 
        && length args == inner_arg_count
    isInnerCall _ = 
      return False
      
  simplifyAndExtract :: forall m . Env.Readable m =>  
    Set Index -> Term -> Env.AlsoTrack Term m Term
  simplifyAndExtract gen_vars term = do
    term' <- lift (run term)
    can_extract <- extractable
    if not can_extract
    then return term'
    else do
      outer_f <- ask
      lift
        . Env.alsoTrack (outer_f, gen_vars) 
        -- We descend into branches as long as the 'inner_f' function
        -- is not pattern matched by that branch.
        . Term.descendWhileM functionNonMatched extract 
        $ term'
    where
    functionNonMatched :: Term -> Env.AlsoTrack (Term, Set Index) m Bool
    functionNonMatched (Case cse_t _ _) = do
      (_, gen_vars) <- ask
      cse_t
        |> Indices.free
        |> Set.intersection gen_vars
        |> Set.null
        |> return
    functionNonMatched _ =
      return False
    
    extractable :: Env.AlsoTrack Term m Bool
    extractable = do
      outer_f <- ask
      ty <- Err.noneM (Typing.typeOf outer_f)
      let (args, ret) = flattenPi ty
      return 
        $ length args == 1
        && isInd ret 
        && not (Term.isRecursiveInd ret)
  
    extract :: Term -> Env.AlsoTrack (Term, Set Index) m Term
    extract (flattenApp -> inj@(Inj {}) : args) = do
      args' <- mapM extract args
      return (unflattenApp (inj : args'))
    extract term = do
      (outer_f, gen_vars) <- ask
      let gen_vars' = term
            |> Indices.free
            |> Set.intersection gen_vars
            |> Set.toList
      mby_t <- term
        |> concatEndosM (map (invent run . App outer_f . Var) gen_vars') 
        |> Fail.toMaybe
      case mby_t of 
        Nothing -> return term
        Just t -> do
          ts <- showM t
          t |> return
          --  |> trace ("GOT: " ++ ts) 
            
-}
