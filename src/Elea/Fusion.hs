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
    , fixfixFusion
    , repeatedArgFusion
    , fixfactFusion
    , removeIdFix
    ]
        
{-# INLINEABLE run #-}
run :: forall m . Env.Readable m => Term -> m Term
run term = do
  term' <- Float.run term
  mby_fused <- firstM (map (applyStep term') steps)
  case mby_fused of 
    Nothing -> return (Term.normalised term')
    Just fused -> do
      ts <- showM term
      ts' <- showM term'
      ts'' <- showM fused
      run fused
     --   |> trace ("\n\nUNFUSED:\n\n" ++ ts' ++ "\n\nFINISHED\n\n" ++ ts'') 
  where
  applyStep :: Term -> (Term -> m (Maybe Term)) -> m (Maybe Term)
  applyStep t f = Fold.isoRewriteOnceM Term.restricted withTrace t
    where
    withTrace t = do
      mby_t <- f t 
      case mby_t of
        Nothing -> return Nothing
        Just t' -> do
          ts <- showM t
          ts' <- showM t'
          Just t'
      --      |> trace ("\n\nTRANSFORMED:\n" ++ ts ++ "\n\nTO:\n" ++ ts')
            |> return
            
simpleAndFloat :: Env.Readable m => Term -> m Term
simpleAndFloat term = do
  term' <- Float.run term
  mby_t <- Fold.isoRewriteOnceM Term.restricted floatConstructors term'
  maybe (return term') simpleAndFloat mby_t
  
varEqApply :: Env.Readable m => Term -> m (Maybe Term)
varEqApply t@(Var {}) = Env.matchedWith t
varEqApply _ = return Nothing

removeIdFix :: Env.Readable m => Term -> m (Maybe Term)
removeIdFix fix_t@(Fix _ (Bind _ fix_ty) _) 
  | ([arg_b@(Bind _ arg_ty)], res_ty) <- flattenPi fix_ty
  , Indices.lift arg_ty == res_ty = do
    let ctx = Context.make fix_ty fix_ty
          $ \_ -> Lam arg_b (Var 0)
    Fail.toMaybe (split Float.run fix_t ctx)
removeIdFix _ = 
  return Nothing

floatConstructors :: forall m . Env.Readable m => Term -> m (Maybe Term)
floatConstructors term@(Fix _ fix_b fix_t) 
  | Set.size suggestions == 0 = return Nothing
  | not (floatable || Term.simplifiable term) = return Nothing
  | otherwise = id
      . firstM 
      . map (Fail.toMaybe . split simpleAndFloat term)
      $ Set.toList suggestions
  where
  fix_ty = get boundType fix_b
  (arg_bs, return_ty) = flattenPi fix_ty
  
  absurd_ctx = id
    . Context.make fix_ty fix_ty
    . const
    . unflattenLam arg_bs
    $ Absurd return_ty
  
  floatable :: Bool
  floatable = cons == 1
    || (cons == 0 && suggestions == Set.singleton absurd_ctx)
    where
    cons = Set.size (returnCons fix_t)
    
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
        
    suggest (Absurd _) = 
      return (Set.singleton absurd_ctx)
      
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

  
fixfixFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fixfixFusion full_t@(flattenApp -> 
      outer_f@(Fix outer_info outer_b _) : outer_args@(_:_)) 
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Typing.typeOf full_t)
    if not (isInd full_ty)
    then return Nothing
    else fixfix full_ty
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
      | isVar rec_arg = fuse run extract outer_ctx inner_f'
      | otherwise = id
          . Term.generalise (head inner_args) 
            (\_ ctx -> fuse run extract ctx (Indices.lift inner_f'))
          $ outer_ctx
      where
      inner_f' = Term.clearFusedMatches inner_f
      rec_arg = head inner_args
  fixfix _ = 
    return Nothing
    
fixfixFusion _ = 
  return Nothing

  
repeatedArgFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
repeatedArgFusion full_t@(flattenApp -> 
      outer_f@(Fix outer_info outer_b _) : outer_args@(_:_))
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Typing.typeOf full_t)
    if not (isInd full_ty)
    then return Nothing
    else repeatedArg full_ty
  where
  repeatedArg :: Type -> m (Maybe Term)
  repeatedArg full_ty
    | x@(Var {}) <- head outer_args
    , any (== x) (tail outer_args) = do
      full_s <- showM full_t
      outer_ty <- Err.noneM (Typing.typeOf outer_f)
      let ctx = id
            . Context.make outer_ty full_ty
            $ \t -> unflattenApp (t : outer_args)
      Fail.toMaybe (fuse run (\_ _ -> return) ctx outer_f)
  repeatedArg _ =
    return Nothing
    
repeatedArgFusion _ =
  return Nothing
    
    
fixfactFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fixfactFusion full_t@(flattenApp -> 
      outer_f@(Fix outer_info outer_b _) : outer_args@(_:_)) 
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Typing.typeOf full_t)
    if not (isInd full_ty)
    then return Nothing
    else fixFact full_ty
  where     
  fixFact :: Type -> m (Maybe Term)
  fixFact full_ty = do
    matches <- Env.matches
    f_off <- Env.fixpointOffset
    matches
      |> Map.toList
      |> map (fuseMatch f_off)
      |> firstM
    where
    fuseMatch :: Index -> (Term, (Term, Int)) -> m (Maybe Term)
    fuseMatch f_off (match_t, 
              (inj_t@(flattenApp -> Inj inj_n ind_ty : inj_args), m_depth))
      | Just inj_n' <- fusedMatch match_t outer_f =
        if inj_n' == inj_n  
        then return Nothing 
        -- If we have already fused in a different pattern matched
        -- to the same term, then this is an absurd branch.
        -- Not sure if this ever comes up though.
        else return (Just (Absurd full_ty))
        {-
      | isFix (leftmost match_t)
      , match_fix_body `Term.containsUnifiable` outer_f = 
        trace ("\n\nFAIL!!!!!!!\n" ++ show match_t ++ "\n\nWITH\n" ++ show outer_f) (return Nothing)
        -}
      | not (isFix (leftmost match_t)) 
        || not relevantFact =
        -- If you do fix-fact fusion with a fact that contains the
        -- fix within it, you will get an infinite loop. Noob.
       -- || match_fix_body `Term.containsUnifiable` outer_f =

        return Nothing

      | otherwise = do
        outer_ty <- Err.noneM (Typing.typeOf outer_f)
        let ctx = Context.make outer_ty full_ty buildContext
        -- We add the new fused matches to the info of our existing 
        -- fixpoint, since this will be carried over to the fixpoint
        -- that fusion produces.
        let outer_f' = outer_f
              |> addFusedMatch (match_t, inj_n)
              |> addFusedMatches (get fusedMatches match_inf)
            full_t' = unflattenApp (outer_f' : outer_args)
        mby_t <- Fail.toMaybe (fuse run floatInwards ctx outer_f')
        match_s <- showM match_t
        outer_s <- showM full_t
        inj_s <- showM inj_t
        let msg | isJust mby_t = id
                | otherwise = 
                    trace ("\n\nFailed to merge:\n" ++ match_s ++ "\n == " ++ inj_s ++ "\n\nwith:\n" ++ outer_s)
        return
          . msg
          . Just
          $ fromMaybe full_t' mby_t
      where
      match_fix@(Fix match_inf _ match_fix_body) : match_args = 
        flattenApp match_t
      match_vars = Simp.strictVars match_t
      strict_vars = Simp.strictVars full_t
      
      -- A fact is only relevant to this term if there is a shared variable,
      -- no variables that have been bound after the match was made,
      -- and at least one variable exists within the current fusion step.
      relevantFact = shared_var && locally_bound && within_fusion
        where 
        shared_var = id
          . not
          . Set.null
          . Set.intersection match_vars
          $ strict_vars
          
        locally_bound = True
       --   all (>= toEnum m_depth) strict_vars
          
        within_fusion = 
          any (< f_off) strict_vars
      
      buildContext :: Term -> Term
      buildContext gap_t = 
        Case match_t' ind_ty alts
        where
        -- In order for replacement to work, the match context term must not 
        -- get fused with anything. I had a problem with argument order
        -- being scrambled by a pointless fusion step within 
        -- sorted-isort, blocking the replacement step.
        match_fix' = Term.blockSimplification match_fix
        match_t' = unflattenApp (match_fix' : match_args)
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
        
      -- The extraction function that is passed to the Core.fuse
      -- call for fixFact fusion. It floats the fact context
      -- into the correct place in the term for fusion to be applied.
      floatInwards :: forall m . (Env.Readable m, Fail.Monad m) => 
        Index -> Context -> Term -> m Term
      floatInwards fix_f match_ctx = id
        . return
        . Env.trackIndices (fix_f, match_ctx)
        . Fold.isoTransformM Term.restricted floatCtxMatchInwards
        where
        floatCtxMatchInwards :: 
          Term -> Env.TrackIndices (Index, Context) Term
        floatCtxMatchInwards cse_t@(Case outer_t outer_ty outer_alts) = do
            (fix_f, match_ctx) <- ask
            let match_t = getMatchTerm match_ctx
            if not (Unifier.exists match_t outer_t)
            then return cse_t
            else do
              let main_alt' = id
                    . Alt main_bs
                    . Env.trackIndices (fix_f, (match_ctx, outer_ctx))
                    . local (Indices.liftMany (length main_bs))
                    . Fold.isoTransformM Term.restricted pushInwards
                    $ main_t
              return 
                . Case outer_t outer_ty 
                $ left_alts ++ (main_alt':right_alts)
          where
          (left_alts, main_alt:right_alts) = 
            splitAt (fromEnum inj_n) outer_alts
          abs_alts = left_alts ++ right_alts
          Alt main_bs main_t = main_alt
          
          getMatchTerm ctx = match_t
            where
            Case match_t _ _ = Context.apply ctx (Absurd Set)

          outer_ctx = Context.make full_ty full_ty mkCtx
            where
            mkCtx gap_t = Case outer_t outer_ty outer_alts'
              where
              makeAbsurd (Alt bs _) = id
                . Alt bs
                . Indices.liftMany (length bs)
                $ Absurd full_ty
              
              left_alts' = map makeAbsurd left_alts
              right_alts' = map makeAbsurd right_alts
              outer_alts' = left_alts' ++ (main_alt':right_alts')
              main_alt' = 
                Alt main_bs (Indices.liftMany (length main_bs) gap_t)
          
          pushInwards :: Term -> 
            Env.TrackIndices (Index, (Context, Context)) Term
          pushInwards term@(flattenApp -> Var var_f : args)
            | length args == length outer_args = do
              (fix_f, (match_ctx, outer_ctx)) <- ask
              let inner_match = Context.apply outer_ctx term
                  outer_match = Context.apply match_ctx (Var fix_f)
              if var_f == fix_f
                && Unifier.exists outer_match inner_match
              then return inner_match
              else return term
          pushInwards other = 
            return other
            
        floatCtxMatchInwards other = 
          return other

    fuseMatch _ _ = 
      return Nothing
  
fixfactFusion _ =
  return Nothing

extract :: forall m . Env.Readable m =>  
  Index -> Context -> Term -> Env.AlsoTrack Term m Term
extract inner_f _ term = do
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
      lift (foldrM (extractContext outer_f) term gen_vars)
      where
      extractContext :: Term -> Index -> Term -> m Term
      extractContext outer_f gen_var term = id
        . Fail.withDefault term
        . invent run (App outer_f (Var gen_var))
        $ term

