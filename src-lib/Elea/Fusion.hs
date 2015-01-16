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
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

type FusionM m = (Defs.Read m, Env.All m, Discovery.Tells m, Memo.Can m)
 
{-# SPECIALISE run :: Term -> Fedd Term #-}

run :: forall m . FusionM m => Term -> m Term
run = Fold.rewriteStepsM all_steps
  where
  all_steps :: [Term -> MaybeT m Term]
  all_steps = Fission.steps ++
    [ const Fail.here
    , expressMatch
    , repeatedArg
    , fixfix
    , fixMatch
    , decreasingFreeVars
    , matchFix 
    ]
    
    
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
        

-- | Apply any rewrites defined by pattern matches
expressMatch :: forall m . (Fail.Can m, Env.MatchRead m) => Term -> m Term
expressMatch term@(App (Fix {}) _) = do
  ms <- Env.findMatches ((== term) . fst)
  Fail.when (length ms == 0)
  if length ms > 1
  -- If we have more than one potential constructor match then we are down
  -- an unreachable branch
  then return (Unr (Type.get term))
  else let [(_, cons_t)] = ms in return cons_t
expressMatch _ = Fail.here

{-
-- | Any pattern matches over fixpoints which would have their fixpoints
-- unfolded after this pattern match, will be expressed within that branch
-- as a constraint.
expressUnfoldingMatch :: forall m . (Fail.Can m, Env.MatchRead m)
  => Term -> m Term
expressUnfoldingMatch cse_t@(Case (Var x) alts) = do
  Fail.unless (Type.has cse_t)
  ms <- Env.findMatches potentialMatch
  alts' <- Fail.choose (map express ms)
  return (Case (Var x) alts')
  where
  result_ty = Type.get cse_t
  
  potentialMatch (t, _) = 
    Set.member (Var x) (Eval.strictTerms t)
    && not (Simp.willUnfold t)
   
  express :: Fail.Can m => (Term, Term) -> m [Alt]
  express (match_t, with_t) = do
    Fail.when (all isNothing m_alts)
    return (zipWith fromMaybe alts m_alts)
    where
    m_alts = map expressAlt alts
    
    expressAlt :: Alt -> Maybe Alt
    expressAlt (Alt con bs alt_t) = do
      Fail.unless (Simp.willUnfold match_t')
      return (Alt con bs alt_t')
      where
      liftHere :: Indexed a => a -> a
      liftHere = Indices.liftMany (length bs)
      
      match_c = Constraint.fromMatch (match_t', liftHere with_t)
      
      alt_t' = id
        . Simp.run 
        $ Constraint.apply match_c (alt_t, result_ty)
      
      match_t' = id               
        . Indices.replaceAt (liftHere x) (Term.altPattern con)
        $ liftHere match_t
        
expressUnfoldingMatch _ = 
  Fail.here
  -}
  
        
-- | Uses fixpoint fusion on a fix with a fix as a decreasing argument.
fixfix :: forall m . (FusionM m, Fail.Can m) => Term -> m Term

-- ofix means "outer fixpoint", oargs is "outer arguments"
fixfix oterm@(App ofix@(Fix {}) oargs) 
  -- Check this is not a partially applied fixpoint
  -- since I'm not sure what happens in this case.
  | Term.inductivelyTyped oterm = do
    oargs' <- mapM Term.revertEnvMatches oargs
    id
      -- Broadcast the discovery
      . Discovery.equalsM (App ofix oargs')
      -- Pick the first one which does not fail
      . Fail.choose
      -- Run fixfixArg on every decreasing fixpoint argument position
      . map (fixfixArg oargs')
      . filter (Term.isFix . Term.leftmost . (oargs' !!))
      $ Term.decreasingArgs ofix
  where
  -- Run fixfix fusion on the argument at the given position
  fixfixArg :: [Term] -> Int -> m Term
  fixfixArg oargs arg_i =
    -- Generalise the arguments of the outer fixpoint
    Term.generaliseArgs oterm outerGeneralised
    where
    oterm = App ofix oargs
    
    outerGeneralised :: Indices.Shift -> Term -> m Term
    outerGeneralised shiftOuter (App ofix' oargs') =
      -- Generalise the arguments of the inner fixpoint
      Term.generaliseArgs ifix_t innerGeneralised 
      where
      ifix_t = shiftOuter (oargs !! arg_i)
      
      innerGeneralised :: Indices.Shift -> Term -> m Term
      innerGeneralised shiftInner (App ifix iargs) = do 
        let ifix_name = get Type.bindLabel (Term.binding ifix)
            ofix_name = get Type.bindLabel (Term.binding ofix)
        Fail.when (ifix_name == "build" && ofix_name == "flat")
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
matchFix :: forall m . (FusionM m, Fail.Can m, Env.MatchRead m) 
  => Term -> m Term
  
matchFix outer_t@(App fix@(Fix fix_info _ _) args) = do
  -- Check that we don't have a partially applied fixpoint.
  -- Could mess things up, and not a case worth considering I think.
  Fail.unless (Term.inductivelyTyped outer_t)
  
  useful_matches <- Env.findMatches usefulMatch

  {-
  when (null useful_matches) $ do
    outer_s <- showM outer_t
    ms <- Env.matches
    ms_s <- showM ms
    strict_s <- showM (map (Eval.strictTerms . fst) ms)
    trace ("\n[match-fix] no shared between:\n" ++ outer_s ++ "\n" ++ ms_s ++ "\nSTRICT:\n" ++ strict_s) 
      Fail.here
    -}
    
  Fail.when (null useful_matches)
  
  extra_matches <- Env.findMatches (overlappingMatch (map fst useful_matches))
  let matches = nubOrd (useful_matches ++ extra_matches)
      constraints = map Constraint.fromMatch matches
      constraint_set = Set.fromList constraints
        
  -- Memoise our efforts for efficiency
  Memo.constraintFusion constraint_set outer_t $ do
    -- Use dynamic checking to see if these constraints could collapse 
    -- the fixpoint to a constaint, and fail if they don't.
    let mby_constraints' = Constraint.unfoldAll constraints
        cons_map = id
          . Map.fromList
          . map (\(Constraint con t) -> (t, con))
          $ fromJust mby_constraints'
        mby_con = Map.lookup outer_t cons_map
        
    case (mby_constraints', mby_con) of
      (Nothing, _) -> return (Unr result_ty)
      (_, Just con)
        | result_ty == Type.get con -> return (Con con)
        
      (Just constraints', Nothing) -> do
        
      let constraint_set' = Set.fromList constraints'
          mby_const = Checker.constrainedToConstant constraint_set' outer_t
          
      -- DEBUG
      cons_s <- showM constraint_set'
      const_s <- showM mby_const
      outer_s <- showM outer_t
      
      -- We need to invoke failure with this debug trace message
      -- more than once within this function
      let fusionFailed = do
            let msg = "\n[match-fix fusion] failed."
                  ++ "\nconstraints:\n" ++ cons_s
                  ++ "\n\ntarget:\n" ++ outer_s
                  ++ "\n\nconstant:\n" ++ const_s 
            trace msg Fail.here
      
      let msg1 = "\n[match-fix fusion] hypothesised that:\n" ++ cons_s 
            ++ "\n\ncollapses:\n" ++ outer_s 
            ++ "\n\ninto constant term:\n" ++ const_s
            
      when (isNothing mby_const) Fail.here -- fusionFailed 
      let Just const_t = mby_const
    
      -- We apply match-fix fusion for every match
      fused_t <- id
        . trace msg1
        $ fuseConstraints constraints' outer_t
        
      when (fused_t /= const_t) fusionFailed
      
      -- Express all the matches we fused in as a single constraint,
      -- so we can show which term we fused together to make 'fused_t',
      -- so we can output it with 'Discovery.equals'
      let all_constraints = 
            concatMap (Constraint.toContext result_ty) constraints'
          from_t = Context.apply all_constraints outer_t
      Discovery.equals from_t fused_t
      return fused_t
  where
  result_ty = Type.get outer_t
  
  -- Whether a pattern match should be fused into the current fixpoint.
  -- We also check that one of the strict arguments of each term match.
  usefulMatch :: (Term, Term) -> Bool
  usefulMatch (match_t@(App (Fix {}) m_args), _) =
    not (Set.null shared_terms)
    where
    shared_terms = 
      (Set.intersection `on` Eval.strictTerms) outer_t match_t
  usefulMatch _ = False
  
  overlappingMatch :: [Term] -> (Term, Term) -> Bool
  overlappingMatch useful_matches (match_t@(App (Fix {}) m_args), _) =
    all Term.isSimple m_args
    && overlapsWith outer_t
    && any overlapsWith useful_matches
    where
    overlapsWith with_t = id
      . not
      . Set.null
      . Set.intersection (Eval.strictTerms match_t)
      . Set.map Var
      $ Indices.free with_t
  overlappingMatch _ _ = False
  
  fuseConstraints :: [Constraint] -> Term -> m Term
  fuseConstraints cons term =
    -- Using 'foldl' is important here, since 'useful_matches' will be the 
    -- firsts ones in the list and they need to be fused in first,
    -- otherwise the 'extra_matches' won't fuse properly, since they'll
    -- have nothing to fuse into.
    foldlM fuse term cons
    where
    fuse :: Term -> Constraint -> m Term
    fuse with_t _
      | (not . isFix . leftmost) with_t = return with_t
    fuse with_t (Constraint con match_t) =
      -- Generalise any function calls
      Term.generaliseTerms gen_terms (match_t, with_t) generalised 
      where
      App (Fix {}) w_args = with_t
      App (Fix {}) m_args = match_t
      
      gen_terms =
        concatMap (Term.collect isGenTerm) (w_args ++ m_args)
        where
        isGenTerm (App f (_:_)) = isFix f || isVar f
        isGenTerm _ = False
        
      generalised _ (match_t, with_t) = id
        . liftM (fromMaybe with_t)
        . Fail.catch
        $ Fix.constraintFusion run cons_here with_t
        where 
        cons_here = Constraint.make con match_t 
        
        
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
  
  
fixMatch :: forall m . (FusionM m, Fail.Can m, Env.MatchRead m) 
  => Term -> m Term
fixMatch inner_t@(App (Fix {}) args) = do
  arg_ms <- Env.findMatches argVarMatch
  Fail.when (null arg_ms)
  fuseMatch (head arg_ms)
  where
  -- Any pattern match which has a pattern variable as an argument to 
  -- the inner fixpoint.
  argVarMatch :: (Term, Term) -> Bool
  argVarMatch (App (Fix {}) _, App (Con {}) xs) = 
    any (`elem` args) xs
  argVarMatch _ = False
  
  -- Fuse the fixpoint of that match
  fuseMatch :: (Term, Term) -> m Term
  fuseMatch match@(App match_fix _, App (Con p_con) p_args) = 
    Fix.fusion simplify ctx match_fix
    where 
    ctx = Context.make (buildContext inner_t match)
    Just inner_i = findIndex (`elem` p_args) args
    Just pat_i = findIndex (== (args !! inner_i)) p_args 
    orig_t = Context.apply ctx (Var 0)
    
    buildContext :: Term -> (Term, Term) -> Term -> Term
    buildContext 
        inner_t@(App _ args)
        (App _ match_args, App (Con p_con) p_args) 
        gap_t = 
      Case (App gap_t match_args) alts
      where
      pat_var = arguments (Term.altPattern p_con) !! pat_i
      alts = id
        . map makeAlt
        . Type.constructors
        $ get Type.constructorOf p_con
      
      makeAlt :: Constructor -> Alt
      makeAlt con = Alt con alt_bs alt_t
        where
        App inner_f' args' = Indices.liftMany (length alt_bs) inner_t
        inner_t' = App inner_f' (setAt inner_i pat_var args')
        
        alt_bs = Type.makeAltBindings con
        alt_t | con /= p_con = Unr (Type.get inner_t)
              | otherwise = inner_t'
              
          
    -- Our custom inner simplification which will get run during fixpoint fusion.
    -- It runs the simplifier then expresses the pattern match wherever possible.
    simplify :: Term -> m Term
    simplify term = do
      term' <- run term
      Env.alsoTrack 0 
        $ Fold.transformM express term'
      where
      express :: Term -> Env.AlsoTrack Index m Term
      express term@(App (Fix {}) i_args)
        | Unifier.exists term inner_t = do
          fix_f <- Env.tracked
          ms <- Env.findMatches (correctMatch fix_f)
          if null ms
          then return term
          else do
            let ctx = Context.make (buildContext term (head ms))
                term' = Context.apply ctx (Var fix_f)
                orig_t' = Indices.liftMany (succ (enum fix_f)) orig_t
            if Unifier.exists term' orig_t'
            then return term'
            else return term
        where
        correctMatch :: Index -> (Term, Term) -> Bool
        correctMatch fix_f (App (Var f) _, App (Con p_con') p_args') = 
          fix_f == f 
          && p_con == p_con' 
          && p_args' !! pat_i == i_args !! inner_i
        correctMatch _ _ = False
      express other = 
        return other
            
fixMatch _ = Fail.here
          
