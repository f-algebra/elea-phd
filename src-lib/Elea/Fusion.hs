-- | Some term transformation steps that rely on fixpoint fusion.
module Elea.Fusion
(
  FusionM, run
)
where

import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Monad.Fedd ( Fedd )
import qualified Elea.Checker as Checker
import qualified Elea.Fixpoint as Fix
import qualified Elea.Inventor as Invent
import qualified Elea.Constraint as Constraint
import qualified Elea.Evaluation as Eval
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Simplifier as Simp
import qualified Elea.Fission as Fission
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.Fusion.Class as Fusion
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

type FusionM m = (Defs.Read m, Env.Read m, Discovery.Tells m, Fusion.Memo m)
 
{-# SPECIALISE run :: Term -> Fedd Term #-}

run :: FusionM m => Term -> m Term
run = runSteps (map Type.checkStep (fission_steps ++ fusion_steps))
  where
  fusion_steps =
    [ const Fail.here
    , Fold.rewriteOnceM repeatedArg
    , Fold.rewriteOnceM fixfix
    , Fold.rewriteOnceM decreasingFreeVars
    , mapMaybeT Env.trackMatches . Fold.rewriteOnceM matchFix
    ]
   
  fission_steps =
    map Fold.rewriteOnceM Fission.steps
    
    
runSteps :: FusionM m => 
    [Term -> MaybeT m Term] -> Term -> m Term
runSteps steps term = do
  -- Make sure the term has had all non-fixpoint based simplifications run
  -- before we try the more advanced fixpoint based one, some of which rely
  -- on the term being in some normal form 
  -- because of these earlier simplifications
  let term' = Simp.run term
  
  -- Fixpoint steps are heavyweight. It is faster to make sure they are
  -- only applied one at a time.
  mby_term'' <- id
    . runMaybeT 
    . Fail.choose 
    $ map ($ term') steps
  
  case mby_term'' of
    Nothing -> return term'
    Just term'' -> do
      ts'' <- showM term''
      ts' <- showM term'
      id
     --   . trace ("\nMAIN LOOP FROM: " ++ ts' ++ "\nINTO: " ++ ts'')
        $ run term''


-- | Uses fixpoint fusion on a fix with a fix as a decreasing argument.
fixfix :: forall m . (FusionM m, Fail.Can m) => Term -> m Term

-- ofix means "outer fixpoint", oargs is "outer arguments"
fixfix oterm@(App ofix@(Fix {}) oargs) 
  -- Check this is not a partially applied fixpoint
  -- since I'm not sure what happens in this case.
  | Term.inductivelyTyped oterm = id
      -- Broadcast the discovery
      . Discovery.equalsM oterm
      -- Pick the first one which does not fail
      . Fail.choose
      -- Run fixfixArg on every decreasing fixpoint argument position
      . map fixfixArg
      . filter (Term.isFix . Term.leftmost . (oargs `nth`))
      $ Term.decreasingArgs ofix
  where
  -- Run fixfix fusion on the argument at the given position
  fixfixArg :: Int -> m Term
  fixfixArg arg_i =
    -- Generalise the arguments of the outer fixpoint
    Term.generaliseArgs oterm outerGeneralised
    where
    outerGeneralised :: Indices.Shift -> Term -> m Term
    outerGeneralised shiftOuter (App ofix' oargs') =
      -- Generalise the arguments of the inner fixpoint
      Term.generaliseArgs ifix_t innerGeneralised 
      where
      ifix_t = shiftOuter (oargs `nth` arg_i)
      
      innerGeneralised :: Indices.Shift -> Term -> m Term
      innerGeneralised shiftInner (App ifix iargs) = do 
        Fix.fusion (simplify ctx) ctx ifix
        where
        -- The context is the outer term, with all variables generalised
        -- except the position of the inner fixpoint, and with this inner 
        -- fixpoint replaced by the gap.
        ctx = Context.make mkCtx
          where
          mkCtx gap_f = id
            . App (shiftInner ofix')
            . replaceAt arg_i (App gap_f iargs) 
            $ map shiftInner oargs' 

  -- The internal simplification used in fixfix fusion.
  -- Recursively runs fusion, and then attempts to extract fixpoints
  -- using invention.
  simplify :: Context -> Term -> m Term
  simplify (Indices.lift -> ctx) term = do
    term' <- run term
    Env.alsoTrack fix_f
      $ Fold.transformM extract term'
    where
    -- The index of the unrolled fixpoint variable
    fix_f = 0
    
    extract :: Term -> Env.AlsoTrack Index m Term
    extract term@(App (Fix {}) args) 
      | any varAppArg args = do
        mby_extr <- runMaybeT extr
        return (fromMaybe term mby_extr)
      where
      varAppArg (App (Var _) _) = True
      varAppArg _ = False
      
      Just (f_term@(App (Var f) _)) = find varAppArg args
      
      extr :: MaybeT (Env.AlsoTrack Index m) Term
      extr = do
        -- Check this is definitely a recursive call
        fix_f' <- Env.tracked
        guard (fix_f' == f)
        
        -- If a unifier exists, then we do not need to do fixpoint extraction
        orig_term <- Env.liftByOffset (Context.apply ctx (Var 0))
        guard (not (Unifier.exists orig_term term))
        
        -- Collect the recursive call in @orig_term@
        -- so we can generalise it
        let [rec_call] = id 
              . Set.elems
              . Env.trackIndices fix_f'
              $ Term.collectM termToGeneralise orig_term 
        
        -- Generalise the recursive calls in both terms to the same variable
        let term' = id
              . Term.replace (Indices.lift f_term) (Var 0) 
              $ Indices.lift term
            orig_term' = id
              . Term.replace (Indices.lift rec_call) (Var 0)
              $ Indices.lift orig_term
              
        f_ty <- Type.getM f_term
                                                                       
        invented_ctx <- id
          . Env.bind (Bind "extrX" f_ty)
          $ Invent.run run orig_term' term'
        
        return
          . Indices.substAt 0 f_term
          $ Context.apply invented_ctx orig_term'
          
        where
        termToGeneralise :: Term -> Env.TrackIndices Index Bool
        termToGeneralise (App (Var f) _) = do
          fix_f <- Env.tracked
          return (f == fix_f)
        termToGeneralise _ = 
          return False

    extract other = 
      return other
      
fixfix _ = Fail.here


-- | If two or more decreasing arguments to a fixpoint are the same 
-- variable, we can sometimes fuse these arguments into one.
repeatedArg :: forall m . (FusionM m, Fail.Can m) => Term -> m Term
  
repeatedArg fix_t@(App fix@(Fix {}) args) 
  | Term.inductivelyTyped fix_t = id
    -- Broadcast the discovery
    . Discovery.equalsM fix_t
    -- Pick the first success
    . Fail.choose
    . map fuseRepeated 
    -- We only care about ones with at least a single repetition
    . filter ((> 1) . length)
    -- Group up all decreasing arguments which are equal
    . groupBy ((==) `on` (args `nth`))
    -- We only care about variable arguments
    . filter (Term.isVar . (args `nth`))
    $ Term.decreasingArgs fix
  where
  fuseRepeated :: [Int] -> m Term
  fuseRepeated arg_is = 
    Term.generaliseArgs fix_t generalised
    where
    generalised :: Indices.Shift -> Term -> m Term
    generalised _ (App fix' args') =
      Fix.fusion (return . Simp.run) (Context.make mkCtx) fix'
      where
      -- The context is the original term, with every argument generalised,
      -- and the repeated arguments in the correct places, and the gap
      -- in the place of the fixpoint (as always).
      mkCtx gap_f = App gap_f args''  
        where
        -- Take the argument we are repeating from the newly generalised ones.
        rep_arg = args' `nth` head arg_is
        
        -- Replace every argument position from the list 
        -- of repeated args 'arg_is' with the same variable.
        args'' = foldr (\i -> replaceAt i rep_arg) args' (tail arg_is) 

repeatedArg _ = Fail.here


-- | Match-Fix fusion. Makes use of an environment which you can read pattern 
-- matches from.
matchFix :: forall m . (FusionM m, Fail.Can m, Env.MatchRead m) => Term -> m Term
  
matchFix outer_t@(App fix@(Fix fix_info _ _) args) = do
  -- We don't try to fuse matches into a term which is not just a fixpoint
  -- with variable or constructor arguments. 
  -- I haven't investigated the behaviour of this
  -- thoroughly enough.
  Fail.unless (all Term.isSimple args)
  
  -- Check that we don't have a partially applied fixpoint.
  -- Could mess things up, and not a case worth considering I think.
  Fail.unless (Term.inductivelyTyped outer_t)
  
  matches <- Env.findMatches usefulMatch
  result_ty <- Type.getM outer_t
  
  Fail.when (null matches)
  
  -- Check whether we have already tried fusing this set of matches into
  -- the fixpoint. This is what 'FixInfo' is for.
  let constraint_set = Set.fromList (map Constraint.fromMatch matches)
  Fail.when (Term.alreadyFused fix_info constraint_set outer_t)
  
  -- Use dynamic checking to see if these constraints could collapse 
  -- the fixpoint to a constaint, and fail if they don't.
  let mby_const = Checker.constrainedToConstant constraint_set outer_t
  if isNothing mby_const
  then do
    outer_s <- showM outer_t
    cons_s <- showM constraint_set
    let s1 = "\n[match-fix fusion] rejected fusing:\n" 
            ++ cons_s ++ "\ninto:\n" ++ outer_s
    return 
      . trace s1
      $ Term.addFusedMatches constraint_set outer_t
  else do
    let Just const = mby_const
    
    -- DEBUG
    cons_s <- showM constraint_set
    const_s <- showM const
    outer_s <- showM outer_t
    let s1 = "\n[match-fix fusion] hypothesised that:\n" ++ cons_s 
          ++ "\n\ncollapses:\n" ++ outer_s 
          ++ "\n\ninto constant term:\n" ++ const_s
  
    -- We apply match-fix fusion for every match
    (fused_t, fused_matches) <- id
      . trace s1
      . runWriterT
      $ foldrM fuseMatch outer_t matches
      
    -- Check to see whether any match-fix fusion steps succeeded.
    Fail.when 
      $ fused_matches == []
    
    -- If match-fix fusion has failed we save the list of matches we attempted
    -- to fuse in.
    if not (matchFixSuccess fused_t)
    then return (Term.addFusedMatches constraint_set outer_t)
    else do
      -- Express all the matches we fused in as a single constraint,
      -- so we can show which term we fused together to make 'fused_t',
      -- so we can output it with 'Discovery.equals'
      let all_constraints = 
            concatMap (Constraint.matchContext result_ty) fused_matches
          from_t = Context.apply all_constraints outer_t 
          fused_t' = Constraint.removeAll fused_t
      Discovery.equals from_t fused_t'
      return fused_t'
  where
  -- Whether a pattern match should be fused into the current fixpoint.
  -- We also check that one of the strict arguments of each term match.
  usefulMatch match_t@(App (Fix {}) m_args) =
    all Term.isSimple m_args 
    && not (Set.null shared_terms)
    where
    shared_terms = 
      Set.intersection (Eval.strictTerms outer_t) (Eval.strictTerms match_t)
  usefulMatch _ = False
  
  -- Whether a term does not contain any fixpoints which have constraints.
  -- A pretty ad hoc way of detecting that match-fix fusion has succeeded.
  -- This was mostly picked because it made examples work. Bleh.
  matchFixSuccess :: Term -> Bool
  matchFixSuccess = not . Fold.any constrainedFix
    where
    constrainedFix (Fix _ _ fix_t) =
      Fold.any Constraint.is fix_t
    constrainedFix _ = False
  
  -- Fuse a pattern match into a fixpoint.
  -- Typed so that we can apply it using foldrM over the list of matches.
  -- If fusion fails it returns the original term.
  -- Will write 'True' if fusion ever succeeds.
  fuseMatch :: (Term, Term) -> Term -> WriterT [(Term, Term)] m Term
  fuseMatch (match_t, con_t) term =
    -- Generalise any uninterpreted function calls
    Term.generaliseUninterpreted (match_t, term) generalised 
    where
    Con con = leftmost con_t
    
    generalised _ (match_t, term) = do
      mby_fused <- id
        . lift
        . Fail.catch 
        $ Fix.constraintFusion (return . Simp.run) constr term
        
      case mby_fused of
        Nothing -> return term
        Just fused -> do
          tell [(match_t, con_t)]
          return fused
      where 
      constr = Constraint.make con match_t 
        
matchFix _ = Fail.here


-- | Find all decreasing arguments to a fixpoint which are also free within
-- that fixpoint. Express these internal arguments as arguments to the fixpoint
-- itself and then do fusion on the repeating variables.
-- For example, the second argument of addition is constant and will be
-- moved into the definition of the fixpoint itself. 
-- So to simplify @add x x@ we fuse the context @fun f -> f x x@ with
-- the fixpoint @add@ having reexpressed the inner argument as an 
-- argument to the fixpoint.
decreasingFreeVars :: forall m . (FusionM m, Fail.Can m) => Term -> m Term

decreasingFreeVars orig_t@(App fix@(Fix {}) orig_args) = do
  Fail.unless (Term.inductivelyTyped orig_t)
  Fail.unless (length dec_free_args > 0)
  
  -- Generalise all the arguments first, the return value of this will
  -- ungeneralise the arguments again
  Discovery.equalsM orig_t
    $ Term.generaliseArgs orig_t generalised
  where
  -- The variable arguments we should attempt this technique on.
  -- They must be a decreasing argument, and free within the fixpoint itself. 
  dec_free_args :: [Int]
  dec_free_args = filter isFreeVar (Term.decreasingArgs fix)
    where
    isFreeVar arg_i 
      | Var x <- orig_args `nth` arg_i =
        x `Indices.freeWithin` fix
    isFreeVar _ = False
  
  generalised :: Indices.Shift -> Term -> m Term
  generalised shiftVars (App fix args) = do
    App fix' _ <- Term.expressFreeVariables free_vars fix
    Fix.fusion (simplify fix') ctx fix'
    where
    free_vars = id
      . shiftVars
      . map Term.fromVar 
      . map (orig_args `nth`) 
      $ dec_free_args
    
    ctx = Context.make (\t -> app t args')
      where
      -- The list of arguments with matching variables set.
      -- First we take the expressed free variables, then we append to that the
      -- original arguments (which will now be generalised) but make sure the 
      -- ones that originally matched a free variable are set to still match.
      args' = map Var free_vars 
        ++ foldr setArg args (dec_free_args `zip` free_vars)
        where
        -- Set the given argument position to the given free variable
        -- in a list of argument terms
        setArg :: (Int, Index) -> [Term] -> [Term]
        setArg (i, free_var) = setAt i (Var free_var)
      
  simplify :: forall m . FusionM m => Term -> Term -> m Term
  simplify (Indices.lift -> fix) = id
    . Env.alsoTrack (fix, 0)
    . Fold.transformM express
    . Simp.run
    where
    express :: Term -> Env.AlsoTrack (Term, Index) m Term
    express term@(App (Var f) args) = do
      (fix, fix_f) <- Env.tracked 
      if f /= fix_f
      then return term
      else do
        -- The trick here is to push the constant arguments
        -- back into the fixpoint, so they will not be generalised by the
        -- generaliseArgs call
        let term' = Simp.removeConstArgs (App fix args)
        
        -- Run fixpoint fission with all arguments generalised
        fissioned <- Term.generaliseArgs term' (\_ -> Fission.run)
        
        -- Undo the above replacement of the fix variable with the fixpoint.
        -- Use rewriteM' to check whether the replacement was ever applied
        -- as it will return 'Nothing' if it never was.
        mby_replaced <- runMaybeT (Fold.rewriteM' replace fissioned)

        -- If replacement failed, just return the original term.
        return (fromMaybe term mby_replaced)
      where
      -- Replace the fixpoint term with the fix variable
      -- if we can properly match it to the original fixpoint
      replace :: Term -> MaybeT (Env.AlsoTrack (Term, Index) m) Term
      replace term@(App inner_fix@(Fix {}) args) = do
        (orig_fix, fix_f) <- Env.tracked
      
        -- Check that this fixpoint can have the same argument expressed
        -- as the original one
        Fail.unless (all (Term.isVar . (args !!)) dec_free_args)
        Fail.unless (all (`Indices.freeWithin` inner_fix) free_vars)
        
        -- We reexpress the free-within arguments as variables to the fixpoint
        -- then we can check if this fixpoint matches the given one 
        -- (since that one had its variables expressed too).
        inner_t@(App inner_fix' _) <- 
          Term.expressFreeVariables free_vars inner_fix  
        Fail.unless (inner_fix' == orig_fix)
        
        -- If it does we can replace it with the fix variable again
        return (App (Var fix_f) (map Var free_vars ++ args))
        where
        free_vars = map (Term.fromVar . (args !!)) dec_free_args
        
      replace _ = 
        Fail.here
        
    express term = 
      return term
        
decreasingFreeVars _ =
  Fail.here
          
