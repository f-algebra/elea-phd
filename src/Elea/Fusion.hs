-- | Some term transformation steps that rely on fixpoint fusion.
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
import qualified Elea.Fixpoint as Fix
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Context as Context
import qualified Elea.Unifier as Unifier
import qualified Elea.Simplifier as Simp
import qualified Elea.Fission as Fission
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

run :: Env.Readable m => Term -> m Term
run term = do
  -- Make sure the term has had all non-fixpoint based simplifications run
  -- before we try the more advanced fixpoint based one, some of which rely
  -- on the term being in some normal form 
  -- because of these earlier simplifications
  term' <- Simp.run term
  
  -- Fixpoint steps are heavyweight. It is faster to make sure they are
  -- only applied one at a time.
  mby_term'' <- id
    . runMaybeT 
    . Fail.choose 
    $ map ($ term') (fission_steps ++ fusion_steps)
  
  maybe (return term') run mby_term''
  where
  fusion_steps =
    [ const Fail.here
    , Fold.rewriteOnceM repeatedArg
    , Fold.rewriteOnceM fixfix
    , mapMaybeT Env.trackMatches . Fold.rewriteOnceM matchFix
    ]
   
  fission_steps = 
    map Fold.rewriteOnceM Fission.steps


-- | Uses fixpoint fusion on a fix with a fix as a decreasing argument.
fixfix :: forall m . (Env.Readable m, Fail.Can m) => Term -> m Term

-- ofix means "outer fixpoint", oargs is "outer arguments"
fixfix oterm@(App ofix@(Fix {}) oargs) = id
  -- Pick the first one which does not fail
  . Fail.choose
  -- Run fixfixArg on every decreasing fixpoint argument position
  . map fixfixArg
  . filter (Term.isFix . Term.leftmost . (oargs !!))
  $ Term.decreasingArgs ofix
  where
  -- Run fixfix fusion on the argument at the given position
  fixfixArg :: Int -> m Term
  fixfixArg arg_i =
    -- Generalise the arguments of the outer fixpoint
    Term.generaliseArgs oterm outerGeneralised
    where
    outerGeneralised :: (Term -> Term) -> Term -> m Term
    outerGeneralised liftOuter (App ofix' oargs') =
      -- Generalise the arguments of the inner fixpoint
      Term.generaliseArgs ifix_t innerGeneralised 
      where
      ifix_t = liftOuter (oargs !! arg_i)
      
      innerGeneralised :: (Term -> Term) -> Term -> m Term
      innerGeneralised liftInner (App ifix iargs) = do 
        Fix.fusion simplify (Context.make mkCtx) ifix
        where
        -- The context is the outer term, with all variables generalised
        -- except the position of the inner fixpoint, and with this inner 
        -- fixpoint replaced by the gap.
        mkCtx gap_f = id
          . App (liftInner ofix')
          . replaceAt arg_i (App gap_f iargs) 
          $ map liftInner oargs' 

  -- The internal simplification used in fixfix fusion
  simplify :: Index -> Context -> Term -> m Term
  simplify _ _ = run
  
fixfix _ = Fail.here


-- | If two or more decreasing arguments to a fixpoint are the same 
-- variable, we can sometimes fuse these arguments into one.
repeatedArg :: forall m . (Env.Readable m, Fail.Can m) => Term -> m Term
repeatedArg fix_t@(App fix@(Fix {}) args) = id
  -- Pick the first success
  . Fail.choose
  . map fuseRepeated 
  -- We only care about ones with at least a single repetition
  . filter ((> 1) . length)
  -- Group up all decreasing arguments which are equal
  . groupBy ((==) `on` (args !!))
  -- We only care about variable arguments
  . filter (Term.isVar . (args !!))
  $ Term.decreasingArgs fix
  where
  fuseRepeated :: [Int] -> m Term
  fuseRepeated arg_is = 
    Term.generaliseArgs fix_t generalised
    where
    generalised :: (Term -> Term) -> Term -> m Term
    generalised _ (App fix' args') =
      Fix.fusion simplify (Context.make mkCtx) fix'
      where
      -- The context is the original term, with every argument generalised,
      -- and the repeated arguments in the correct places, and the gap
      -- in the place of the fixpoint (as always).
      mkCtx gap_f = App gap_f args''  
        where
        -- Take the argument we are repeating from the newly generalised ones.
        rep_arg = args' !! head arg_is
        
        -- Replace every argument position from the list 
        -- of repeated args 'arg_is' with the same variable.
        args'' = foldr (\i -> replaceAt i rep_arg) args' (tail arg_is) 
        
  -- No need for fixpoint simplifications within repeated argument fusion
  -- (at least, not that I've ever observed)
  simplify :: Index -> Context -> Term -> m Term
  simplify _ _ = Simp.run

repeatedArg _ = Fail.here


-- | Match-Fix fusion. Makes use of an environment which you can read pattern 
-- matches from.
matchFix :: forall m . (Env.MatchReadable m, Fail.Can m) => Term -> m Term
matchFix outer_t@(App fix@(Fix fix_info fix_b fix_t) args) = do
  -- We don't try to fuse matches into a term which is not just a fixpoint
  -- with variable arguments. I haven't investigated the behaviour of this
  -- thoroughly enough.
  Fail.unless (all isVar args)
  
  matches <- Env.findMatches usefulMatch
  result_ty <- Type.get outer_t
  Fail.choose (map (fuseMatch result_ty) matches)
  where
  dec_args = id
    . Set.fromList
    . map (args !!) 
    $ Term.decreasingArgs fix
    
  -- Whether a pattern match should be fused into the current fixpoint.
  -- We check that one of the decreasing arguments matches a decreasing
  -- argument of the fixpoint.
  usefulMatch (App m_fix@(Fix {}) m_args) 
    | all isVar m_args = id
      . not 
      . Set.null
      . Set.intersection dec_args
      . Set.fromList
      . map (m_args !!)
      $ Term.decreasingArgs m_fix
  usefulMatch _ = False
  
  -- The inner simplification used in match fix fusion
  simplify :: Index -> Context -> Term -> m Term
  simplify _ _ = Simp.run
  
  -- Fuse a pattern match into the outer fixpoint
  fuseMatch :: Type -> (Term, Term) -> m Term
  fuseMatch result_ty (match_t, leftmost -> Con ind con_n) 
    | Just con_n' <- Term.matchedTo fix_info match_t = do
      -- If we have already matched this term to this constructor in this 
      -- fixpoint then we would repeating ourself if we do fusion, so we fail.
      Fail.when (con_n == con_n')
      
      -- Otherwise we are down an absurd branch, since we have matched
      -- a term to two different constructors
      return (Absurd result_ty)
      
    | otherwise = do
      -- If we haven't matched this term to anything yet within the fixpoint
      -- then we can apply fusion
      Fix.fusion simplify context fix'
    where
    cons = Type.unfold ind
    context = Context.make makeContext
    match_fix_info = fixInfo (leftmost match_t)
    
    -- The new fused matches are all fused matches of the original fix,
    -- those of the match we are fusing in, and the match itself.
    -- 'FixInfo' is a monoid, so we can use append here.
    fix_info' = fix_info ++ match_fix_info ++ (FixInfo [(match_t, con_n)])
    fix' = Fix fix_info' fix_b fix_t
    
    -- The context which represents the pattern match, 
    -- viz. return absurd down every branch which is not
    -- the one that was matched, and return the gap down the one that was.
    makeContext gap_f = 
      Case ind match_t alts
      where
      alts = map buildAlt [0..length cons - 1]
      
      buildAlt :: Int -> Alt
      buildAlt alt_n = 
        Alt bs alt_t
        where
        Bind _ con_ty = cons !! alt_n
        bs = map (Bind "X") (init (Type.flatten con_ty))
        
        -- If we are not down the matched branch we return absurd
        alt_t | enum alt_n /= con_n = Absurd result_ty
              | otherwise = Indices.liftMany (elength bs) (App gap_f args)
        
matchFix _ = Fail.here

{-

  

steps :: Env.Readable m => [Term -> m (Maybe Term)]
steps = id
  . map Typing.checkStep
  $ [ removeIdFix 
    , floatConstructors 
 --   , refoldFusion
    , fixfixFusion
    , repeatedArgFusion
    , fixfactFusion
 --   , factfixFusion
    ]

run :: Env.Readable m => Term -> m Term
run = runSteps steps
    
runSteps :: forall m . Env.Readable m => 
  [Term -> m (Maybe Term)] -> Term -> m Term
runSteps steps term = do
  term' <- Float.run term
  mby_fused <- firstM (map (applyStep term') steps)
  case mby_fused of 
    Nothing -> return term'
    Just fused -> do
      ts <- showM term 
      ts' <- showM term'
      ts'' <- showM fused
      run fused
     --   |> trace ("\nFINISHED\n\n" ++ ts'') 
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
        --    |> trace ("\n\nTRANSFORMED:\n" ++ ts ++ "\n\nTO:\n" ++ ts')
            |> return
      

removeIdFix :: Env.Readable m => Term -> m (Maybe Term)
removeIdFix fix_t@(Fix _ (Bind _ fix_ty) _) 
  | ([arg_b@(Bind _ arg_ty)], res_ty) <- flattenPi fix_ty
  , Indices.lift arg_ty == res_ty = do
    let ctx = Context.make fix_ty
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
      . map (Fail.toMaybe . split (runSteps [floatConstructors]) term)
      $ Set.toList suggestions
  where
  fix_ty = get boundType fix_b
  (arg_bs, return_ty) = flattenPi fix_ty
  
  absurd_ctx = id
    . Context.make fix_ty 
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
      
    suggest inj_t@(flattenApp -> Inj inj_n ind_ty : args) = do
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
        . Context.make fix_ty
        . const
        . unflattenLam arg_bs
        . Indices.lowerMany idx_offset
        $ inj_t
        
      gapContext idx_offset gap_n 
        | arg_ty /= ind_ty = mempty
        | otherwise = Set.singleton (Context.make fix_ty mkContext)
        where
        arg_ty = id
          . Term.nthArgument gap_n
          . get boundType
          . (!! fromEnum inj_n)
          $ Typing.unfoldInd ind_ty
          
        (left, _:right) = splitAt (fromEnum gap_n) args
                
        mkContext gap_f = id
          . unflattenLam arg_bs
          . App inj'
          $ left' ++ [gap] ++ right'
          where
          inj' = Indices.lowerMany idx_offset (Inj inj_n ind_ty)
          left' = map (Indices.lowerMany idx_offset) left
          right' = map (Indices.lowerMany idx_offset) right
          
          gap = id
            . app gap_f
            . map (Var . toEnum) 
            $ reverse [0..length arg_bs - 1]
    suggest _ = 
      return mempty
floatConstructors _ = 
  return mzero

-- TODO don't think I need to carry the 'full_ty' parameter through any more
-- since I got rid of that argument to Context.make
fixfixFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fixfixFusion full_t@(App outer_f@(Fix outer_info outer_b _) outer_args)
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Type.get full_t)
    if not (isInd full_ty)
    then return Nothing
    else fixfix (head outer_args) full_ty
  where
  fixfix :: Term -> Type -> m (Maybe Term)
  fixfix fix_arg full_ty
    | isInj (leftmost fix_arg) = do
      mby_t <- Env.matchedFrom fix_arg
      if isNothing mby_t
      then return Nothing
      else fixfix (fromJust mby_t) full_ty
      
    | isFix (leftmost fix_arg) = do
      inner_ty <- Err.noneM (Type.get inner_f)
      let outer_ctx = id
            . Context.make inner_ty
            $ \t -> app outer_f (app t inner_args : tail outer_args)
      runMaybeT
        . Env.alsoTrack outer_f
        $ runFusion outer_ctx
    where
    App inner_f@(Fix {}) inner_args = fix_arg
    
    runFusion :: Context -> Env.AlsoTrack Term (MaybeT m) Term 
    runFusion outer_ctx
      | isVar rec_arg = fuse run extract outer_ctx inner_f'
      | otherwise = id
          . Term.generalise rec_arg
            (\_ ctx -> fuse run extract ctx (Indices.lift inner_f'))
          $ outer_ctx
      where
      inner_f' = inner_f -- Term.clearFusedMatches inner_f
      rec_arg = head inner_args
  fixfix _ _ = 
    return Nothing
    
fixfixFusion _ = 
  return Nothing

  
repeatedArgFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
repeatedArgFusion full_t@(App outer_f@(Fix outer_info outer_b _) outer_args)
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Type.get full_t)
    if not (isInd full_ty)
    then return Nothing
    else repeatedArg full_ty
  where
  repeatedArg :: Type -> m (Maybe Term)
  repeatedArg full_ty
    | x@(Var {}) <- head outer_args
    , any (== x) (tail outer_args) = do
      full_s <- showM full_t
      outer_ty <- Err.noneM (Type.get outer_f)
      let ctx = id
            . Context.make outer_ty
            $ \t -> app t outer_args
      Fail.toMaybe (fuse Float.run (\_ _ -> return) ctx outer_f)
  repeatedArg _ =
    return Nothing
    
repeatedArgFusion _ =
  return Nothing
    
  
fixfactFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
fixfactFusion full_t@(App outer_f@(Fix outer_info outer_b _) outer_args) 
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Type.get full_t)
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
              (inj_t@(flattenApp -> Inj inj_n ind_ty : inj_args), _))
      | Just inj_n' <- fusedMatch match_t outer_f =
        if inj_n' == inj_n  
        then return Nothing 
        -- If we have already fused in a different pattern matched
        -- to the same term, then this is an absurd branch.
        -- Not sure if this ever comes up though.
        else return (Just (Absurd full_ty))

      | not (isFix (leftmost match_t)) || not relevantFact =
        return Nothing

      | otherwise = do
        repeats <- willRepeat
        if repeats 
        then return Nothing
        else do
          outer_ty <- Err.noneM (Type.get outer_f)
          let ctx = Context.make outer_ty buildContext
          -- We add the new fused matches to the info of our existing 
          -- fixpoint, since this will be carried over to the fixpoint
          -- that fusion produces.
          outer_f' <- id
            . return
            . addFusedMatches (get fusedMatches match_inf)
            . addFusedMatch (match_t, inj_n)
            $ outer_f
          let full_t' = app outer_f' outer_args
          mby_t <- Fail.toMaybe 
            $ fuse Float.run floatInwards ctx outer_f'
          match_s <- showM match_t
          outer_s <- showM full_t
          inj_s <- showM inj_t
          let msg | isJust mby_t = id
                  | otherwise = 
                      trace ("\n\nFailed to merge:\n" ++ match_s ++ "\n == " ++ inj_s ++ "\n\nwith:\n" ++ outer_s)
          return
            . msg
            . Just
            . Fold.transform Term.allowSimplification
            $ fromMaybe full_t' mby_t
      where
      match_fix@(Fix match_inf match_b match_fix_body)
        `App` match_args = match_t
      match_vars = Term.strictVars match_t
      strict_vars = Term.strictVars full_t
      
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
          
        within_fusion = True
          -- any (< f_off) strict_vars
          
      willRepeat :: m Bool
      willRepeat = id
        . Env.bind match_b
        . Env.alsoTrack 0 
        . Fold.anyM repeats
        $ match_fix_body
        where
        repeats :: Term -> Env.AlsoTrack Index m Bool
        repeats term@(App (Fix {}) args)
          | Just uni <- Unifier.find full_t term = do
            ms <- Env.matches
            f_var <- Env.tracked
            return 
              . any isJust
              . map (Unifier.union uni . fromJust)
              . filter isJust
              . map (Unifier.find (app (Var f_var) match_args))
              . filter ((== Var f_var) . leftmost) 
              $ Map.keys ms
        repeats _ = 
          return False
      
      buildContext :: Term -> Term
      buildContext gap_t = 
        Case match_t' ind_ty alts
        where
        -- In order for replacement to work, the match context term must not 
        -- get fused with anything. I had a problem with argument order
        -- being scrambled by a pointless fusion step within 
        -- sorted-isort, blocking the replacement step.
        match_fix' = Term.blockSimplification match_fix
        match_t' = app match_fix' match_args
        ind_cons = Typing.unfoldInd ind_ty
        alt_ns = map toEnum [0..length ind_cons - 1]
        alts = zipWith buildAlt alt_ns ind_cons
        
        buildAlt :: Nat -> Bind -> Alt
        buildAlt alt_n bind = Alt bs alt_t
          where
          bs = fst . flattenPi . get boundType $ bind
          liftHere = Indices.liftMany (length bs)
          
          arg_idxs = map (fromVar . liftHere) inj_args
          new_vars = map (Var . toEnum) (reverse [0..length bs - 1])
          
          alt_t 
            | alt_n /= inj_n = liftHere (Absurd full_ty)
            | otherwise = id
                . concatEndos (zipWith replaceAt arg_idxs new_vars)
                . liftHere
                $ app gap_t outer_args
        
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
          (fix_f, match_ctx) <- Env.tracked
          let match_t = getMatchTerm match_ctx
          if not (Unifier.exists match_t outer_t)
            -- Without this second check sorted-flatten wasn't simplifying
            -- properly, far too complicated to actually figure out
            -- why this is.
            || Unifier.exists (Context.apply match_ctx (Var fix_f)) cse_t
          then return cse_t
          else do
            let main_alt' = id
                  . Alt main_bs
                  . Env.trackIndices (fix_f, (match_ctx, outer_ctx))
                  . Env.liftTrackedMany (length main_bs)
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

          outer_ctx = Context.make full_ty mkCtx
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
          pushInwards term@(App (Var var_f) args)
            | length args == length outer_args = do
              (fix_f, (match_ctx, outer_ctx)) <- Env.tracked
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
  
  
factfixFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
factfixFusion full_t@(App inner_f@(Fix _ _ inner_f_body) inner_args)
  | Term.simplifiable full_t = do
    full_ty <- Err.noneM (Type.get full_t)
    if not (isInd full_ty)
    then return Nothing
    else factFix full_ty
  where     
  factFix :: Type -> m (Maybe Term)
  factFix full_ty = do
    matches <- Env.matches
    matches
      |> Map.toList
      |> map fuseMatch
      |> firstM
    where
    fuseMatch :: (Term, (Term, Int)) -> m (Maybe Term)
    fuseMatch (match_t, (flattenApp -> Inj inj_n ind_ty : inj_args, _))
      | isFix (leftmost match_t)
      , length shared_idxs == 1 
      -- Since caseRec in Floating favours fix-fact fusion, we need to do
      -- this clumsy check to prevent infinite loops.
      , not (inner_f_body `Term.containsUnifiable` match_f) = do
        match_ty <- Err.noneM (Type.get match_f)
        let ctx = Context.make match_ty buildContext
        mby_res <- runMaybeT (runFusion ctx)
        full_s <- showM full_t
        res_s <- mapM showM mby_res
        match_s <- showM match_t
        let msg | isJust mby_res = id -- trace ("!!!\n" ++ match_s ++ "\n+++\n" ++ full_s ++ "\n===>\n" ++ fromJust res_s)
                | otherwise = id
        
        msg $ return mby_res
      where
      App match_f match_args = match_t
      strict_vars = Term.strictVars full_t
      shared_idxs@(~[shared_idx]) = 
        findIndices ((`Set.member` strict_vars) . fromVar) inj_args
      new_arg = toEnum (length inj_args - (shared_idx + 1))
      rec_arg = head match_args
      
      runFusion :: Context -> MaybeT m Term
      runFusion outer_ctx
        | isVar rec_arg = fuse Float.run floatInwards outer_ctx match_f
        | otherwise = id
            . Term.generalise rec_arg
              (\_ ctx -> fuse Float.run floatInwards ctx (Indices.lift match_f))
            $ outer_ctx
      
      buildContext :: Term -> Term
      buildContext gap_t = 
        Case cse_t ind_ty alts
        where
        cse_t = app gap_t match_args
        ind_cons = Typing.unfoldInd ind_ty
        alt_ns = map toEnum [0..length ind_cons - 1]
        alts = zipWith buildAlt alt_ns ind_cons
        
        buildAlt :: Nat -> Bind -> Alt
        buildAlt alt_n bind = Alt bs alt_t
          where
          bs = fst . flattenPi . get boundType $ bind
          liftHere = Indices.liftMany (length bs)
          old_arg = (fromVar . liftHere . (inj_args !!)) shared_idx
          
          alt_t 
            | alt_n /= inj_n = liftHere (Absurd full_ty)
            | otherwise = id
                . Indices.replaceAt old_arg (Var new_arg)
                $ liftHere full_t
          
      -- The extraction function that is passed to the Core.fuse
      -- call for factFix fusion. It floats the fact context
      -- into the correct place in the term for fusion to be applied.
      -- This is just copy-pasta from fix-fact-fusion, with only a few
      -- things changed.
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
          (fix_f, match_ctx) <- Env.tracked
          let full_t = Context.apply match_ctx (Var fix_f)
          if not (leftmost outer_t == Var fix_f)
            || Unifier.exists full_t cse_t
          then return cse_t
          else do
            let main_alt' = id
                  . Alt main_bs
                  . Env.trackIndices (full_t, 0)
                  . Env.liftTrackedMany (length main_bs)
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

          makeInnerMatch offset gap_t =
            Case outer_t' outer_ty' outer_alts'
            where
            liftHere :: Indexed t => t -> t
            liftHere = Indices.liftMany offset
            
            outer_t' = liftHere outer_t
            outer_ty' = liftHere outer_ty
            old_arg = liftHere new_arg
            main_alt' = id
              . Alt main_bs 
              . Indices.replaceAt old_arg (Var new_arg)
              . Indices.liftMany (length main_bs)
              $ gap_t
            
            makeAbsurd (Alt bs _) = id
              . Alt bs
              . Indices.liftMany (length bs)
              $ Absurd full_ty
             
            left_alts' = map makeAbsurd left_alts
            right_alts' = map makeAbsurd right_alts
            outer_alts' = left_alts' ++ (main_alt':right_alts')
          
          pushInwards :: Term -> 
            Env.TrackIndices (Term, Index) Term
          pushInwards term@(App fix@(Fix {}) args) = do
              (full_t, fromEnum -> offset) <- Env.tracked
              let inner_match = makeInnerMatch offset term
              if Unifier.exists full_t inner_match
              then return inner_match 
              else return term
          pushInwards other = 
            return other
            
        floatCtxMatchInwards other = 
          return other

    fuseMatch _ = 
      return Nothing
  
factfixFusion _ =
  return Nothing
  

refoldFusion :: forall m . Env.Readable m => Term -> m (Maybe Term)
refoldFusion cse_t@(Case (Var x) ind_ty alts)
  | Term.variablesUnused cse_t = do
    var_ty <- Err.noneM (Type.get (Var x))
    var_name <- showM (Var x)
    let new_b = Bind (Just ("_" ++ var_name)) var_ty
    Env.bindAt 0 new_b $ do
      full_ty <- Err.noneM (Type.get reverted_t)
      potentials <- id
        . liftM toList
        . Env.alsoTrack 0
        . Fold.isoCollectM Term.restricted (potentialRefold full_ty)
        $ reverted_t
      liftM (fmap (Indices.substAt 0 (Var x)))
        $ firstM (map applyPotential potentials)
  where
  reverted_t = Indices.lift (Term.revertMatches cse_t)
  
  applyPotential :: (Context, Term) -> m (Maybe Term)
  applyPotential (ctx, inner_f) = 
    Fail.toMaybe
      $ fuse Float.run extract ctx inner_f
    where
    extract :: Index -> Context -> Term -> MaybeT m Term
    extract idx ctx = id
      . Env.alsoTrack (idx, ctx)
      . Term.expressMatches expressWhen
      . Term.revertMatches
      where
      expressWhen :: (Term, Term) -> Term -> Term -> 
        Env.AlsoTrack (Index, Context) (MaybeT m) Bool
      expressWhen (Var _, _) _ expressed = do
        (inner_f, ctx) <- Env.tracked
        let full_t = Context.apply ctx (Var inner_f)
        return (Unifier.exists full_t expressed)
      expressWhen _ _ _ = 
        return False
  
  potentialRefold :: Type -> Term -> 
    MaybeT (Env.AlsoTrack Index m) (Context, Term) 
  potentialRefold full_ty inner_t@(App inner_f@(Fix {}) args) = do
    offset <- Env.tracked
    guard (any (== Var (x + offset + 1)) args)
    
    inner_t' <- id
      . MaybeT 
      . return
      . Indices.tryLowerMany (fromEnum offset)
      $ inner_t
      
    guard (Term.occurrences inner_t' reverted_t == 1)
    inner_ty <- id
      . liftM (Indices.lowerMany (fromEnum offset))
      $ Err.noneM (Type.get inner_t)
    let ctx = buildContext inner_ty inner_t' 
    return (ctx, leftmost inner_t')
    where
    buildContext inner_ty inner_t'@(App inner_f' args') = 
      Context.make inner_ty mkCtx
      where 
      -- We replace the matching variables with our new variable created
      -- at the index which will match "offset".
      mkCtx gap_t = 
        Case (Var 0) ind_ty alts
        where
        args'' = 
          map (Indices.replaceAt (x + 1) (Var 0)) args'
        
        Case (Var _) ind_ty alts = 
          Term.replace inner_t' (app gap_t args'') reverted_t
        
  potentialRefold _ _ = mzero
  
refoldFusion _ =
  return Nothing


extract :: forall m . Env.Readable m =>  
  Index -> Context -> Term -> Env.AlsoTrack Term m Term
extract inner_f _ term = do
  can_extract <- extractable
  if not can_extract
  then return term
  else do
    outer_f <- Env.tracked
    lift
      . Env.alsoTrack (outer_f, inner_f) 
      -- We descend into branches as long as the 'inner_f' function
      -- is not pattern matched by that branch.
      . Term.descendWhileM functionNonMatched doExtract 
      $ term
  where
  functionNonMatched :: Term -> Env.AlsoTrack (Term, Index) m Bool
  functionNonMatched (Case cse_t _ _) = do
    (_, inner_f) <- Env.tracked
    return (not (inner_f `Set.member` Indices.free cse_t))
  functionNonMatched _ = 
    return False
  
  extractable :: Env.AlsoTrack Term m Bool
  extractable = do
    outer_f <- Env.tracked
    ty <- Err.noneM (Type.get outer_f)
    let (args, ret) = flattenPi ty
    return 
      $ length args == 1
      && isInd ret 
      && not (Term.isRecursiveInd ret)
  
  doExtract :: Term -> Env.AlsoTrack (Term, Index) m Term
  doExtract (flattenApp -> inj@(Inj {}) : args) = do
    args' <- mapM doExtract args
    return (app inj args')
  doExtract term = do
    (outer_f, inner_f) <- Env.tracked
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
    isInnerCall term@(App (Var f) args) = do
      inner_f <- Env.tracked
      if inner_f /= f 
      then return False
      else do
        ty <- Err.noneM (Type.get term)
        return (not (isPi ty))
    isInnerCall _ = 
      return False
      
    extraction :: [Index] -> Term -> Env.AlsoTrack Term m Term
    extraction gen_vars term = do
      outer_f <- Env.tracked
      lift (foldrM (extractContext outer_f) term gen_vars)
      where
      extractContext :: Term -> Index -> Term -> m Term
      extractContext outer_f gen_var term = id
        . Fail.withDefault term
        . invent run (App outer_f [Var gen_var])
        $ term
-}
