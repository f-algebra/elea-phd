-- | Some term transformation steps that rely on fixpoint fusion.
module Elea.Transform.Fusion
(
  run
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import Elea.Unification ( Unifier )
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Transform as Transform
import qualified Elea.Term.Ext as Term
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Type.Ext as Type
import qualified Elea.Unification as Unifier
import qualified Elea.Transform.Names as Name
import qualified Elea.Transform.Evaluate as Eval
import qualified Elea.Transform.Simplify as Simp
import qualified Elea.Transform.Rewrite as Rewrite
import qualified Elea.Transform.Prover as Prover
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Monad.History as History
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.Memo.Class as Memo
import qualified Elea.Monad.Fusion as Fusion
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Poset as Quasi


type Env m = 
  ( Defs.Read m
  , Env.All m
  , Discovery.Tells m
  , Tag.Gen m
  , History.Env m 
  , Fusion.Env m
  , Memo.Can m
  , Direction.Has m )
  
  
type Step m =
  ( Prover.Step m
  , Env m )
 
  
run :: Env m => Term -> m Term
run = Transform.fix (Transform.compose all_steps)
  where
  all_steps = []
    ++ Eval.transformSteps
    ++ Rewrite.rewriteSteps
    ++ Prover.steps
    ++ Eval.traverseSteps
    ++ Rewrite.expressSteps
    -- ^ Prioritising rewrites over descending into terms
    -- speeds things up a bit
    ++ Simp.steps
    ++ steps
    
  
steps :: Step m => [Term -> m Term]
steps = 
  [ const Fail.here
  , fixfix
  , fixCon
  , accumulation
  , decreasingFreeVar
  , repeatedArg
  , matchFix
  , discoverFold
  ]


fusion :: Step m=> Term -> Term -> m Term
fusion ctx_t fix@(Fix fix_i fix_b fix_t) = id
  . Env.forgetAllMatches 
  . Memo.memo Name.Fusion orig_t $ do
    t_s <- showM orig_t
  
    temp_idx <- Tag.make
    let temp_i = set fixIndex temp_idx fix_i
        temp_fix = Fix temp_i fix_b fix_t
    rewrite_from <- id
      . Simp.runWithoutUnfoldM
      -- We cannot use unfold because it will break fixCon fusion
      -- but we need constArg for free-arg fusion
      . Indices.lift
      $ Term.reduce ctx_t [temp_fix]
      
    Type.assertEqM "[fixfix]" (Indices.lower rewrite_from) orig_t
           
    t_s' <- Env.bind new_fix_b $ showM rewrite_from
   -- t_s'' <- showM (Eval.run (app ctx_t [Term.unfoldFix temp_fix]))
    
    new_fix_t <- id  
      . Env.bind new_fix_b
      . Fusion.local temp_idx rewrite_from 0
      . Transform.continue
      . trace ("\n\n[fusing <" ++ show temp_idx ++ ">] " ++ t_s)
    --  . trace ("\n\n[replacing] " ++ t_s')
   --   . trace ("\n\n[transforming< " ++ show temp_idx ++ ">] " ++ t_s'')
      . Indices.lift
      -- ^ Make room for our new variables we are rewriting to
      $ app ctx_t [Term.unfoldFix temp_fix]
      
    t_s''' <- showM (Eval.run (Fix fix_i new_fix_b new_fix_t))
      
    if not (0 `Indices.freeWithin` new_fix_t) 
    then do 
      when (Set.member temp_idx (Tag.tags new_fix_t)) 
     --   $ trace ("\n\n[fusing <" ++ show temp_idx   ++ ">] " ++ t_s)
        $ trace ("\n\n[failing <" ++ show temp_idx ++ ">] " ++ t_s''')
        $ Fail.here
      Simp.runM
        $ Indices.lower new_fix_t
    else id
      . Rewrite.run
     -- . trace ("\n\n[fusing <" ++ show temp_idx   ++ ">] " ++ t_s)
      . trace ("\n\n[yielding <" ++ show temp_idx ++ ">] " ++ t_s''') 
      . Fix fix_i new_fix_b 
      $ Tag.replace temp_idx orig_idx new_fix_t
  where
  orig_t = Term.reduce ctx_t [fix]
  orig_idx = get fixIndex fix_i
  
  new_fix_b = set Type.bindType (Type.get orig_t) fix_b
      
  
fixCon :: forall m . Step m => Term -> m Term
fixCon orig_t@(App fix@(Fix _ fix_b _) args) = do
  Fusion.checkEnabled
  Direction.requireInc
  -- ^ This is because 'matchVar' only works with Inc
  -- and this step only works with 'matchVar'
  Fail.when (Term.beingFused fix)
  Fail.when (all (isCon . leftmost) dec_args)
  -- ^ Only applicable if unrolling is not
  Fail.when (Type.isRecursive (Type.fromBase (Type.get orig_t)))
  -- ^ I can't envisage this being useful to recursively typed return values
  Fail.choose
    . map (conArg . enum)
    . filter (Term.isCon . Term.leftmost . (args !!))
    $ Term.decreasingArgs fix
  where
  dec_args = map (args !!) (Term.decreasingArgs fix) 
  
  conArg :: Int -> m Term
  conArg con_i = do
    (built_t, full_args) <- Term.buildContext con_i orig_t
    let (built_bs, App (Fix {}) built_xs) = flattenLam (Indices.lift built_t)
        gen_fix = Var (elength built_bs)
        ctx_t = id
          . unflattenLam (fix_b : built_bs)
          $ App gen_fix built_xs
        
    let gen_t = Term.reduce ctx_t (fix:full_args)
    gen_s <- showM gen_t
    History.check Name.ConFusion gen_t $ do
      fused_t <- fusion ctx_t fix
      let new_t = Term.reduce fused_t full_args
      new_s <- showM new_t
      -- tracE [("fixcon input", gen_s), ("fixcon output", new_s)]
      Transform.continue new_t
        
fixCon _ = Fail.here

    
fixfix :: forall m . Step m => Term -> m Term
fixfix o_term@(App o_fix@(Fix fix_i _ o_fix_t) o_args) = do
  -- ^ o_ is outer, i_ is inner
  Fusion.checkEnabled
  Fail.when (Term.beingFused o_fix)
  orig_s <- showM o_term
  Fail.assert ("fixfix given non lambda floated outer fixed-point" 
              ++ show o_fix ++ "\n" ++ show (Type.argumentTypes (Type.get o_fix))
                ++ ", " ++ show (fst (flattenLam o_fix_t)))
   -- . tracE [("fix-fix check", orig_s)]
    $ Term.isLambdaFloated o_fix
    
  o_free_bs <- mapM Env.boundAt o_free_vars
    
  -- Pick the first one which does not fail
  Fail.choose
    -- Run fixfixArg on every decreasing fixpoint argument position
    . map (fixArg o_free_bs . enum)
    . filter (Term.isFix . Term.leftmost . (o_args !!))
    $ reverse o_dec_idxs
  where  
  o_dec_idxs = Term.decreasingArgs o_fix
  o_free_vars = Set.toList (Indices.free o_fix)
  
  fixArg :: [Bind] -> Int -> m Term
  -- Run fix-fix fusion on the argument at the given index
  fixArg o_free_bs arg_i = do
    Fail.when (Term.beingFused i_fix)
    Fail.assert ("fixfix given non lambda floated inner fixed-point " 
                ++ show i_fix)
      $ Term.isLambdaFloated i_fix
    
    (built_ctx, full_args) <- Term.buildContext arg_i o_term
    let (built_bs, built_t) = flattenLam (Indices.lift built_ctx)
        App built_f built_xs = built_t
        Fix {} : i_xs = flattenApp (built_xs !! arg_i)
        gen_fix = Var (elength built_bs)
        built_xs' = setAt arg_i (App gen_fix i_xs) built_xs
        ctx_t = unflattenLam (i_fix_b:built_bs) (App built_f built_xs')

        gen_t = Term.reduce ctx_t [i_fix]
        full_t = Term.reduce ctx_t (i_fix : full_args)
        
 --   gen_s <- showM full_t
  --  gen_s' <- showM (Term.reduce old_ctx_t (i_fix : full_args))
  --  ctx_s' <- showM old_ctx_t
  --  ctx_s <- showM ctx_t
   -- tracE [("fixfix on", gen_s), ("old gen", gen_s'), 
    --       ("context", ctx_s), ("old context", ctx_s')]
    Type.assertEqM "fix-fix created an incorrectly typed context" o_term full_t

    History.check Name.FixFixFusion gen_t $ do
      new_fix <- fusion ctx_t i_fix
      let new_term = app new_fix full_args
      new_term' <- Transform.continue new_term
      ts <- showM new_term'
     -- trace ("\n\n[fixfix yielded]\n" ++ ts)
      return new_term'
    where
    
    i_term@(App i_fix@(Fix _ i_fix_b i_fix_t) i_args) = o_args !! arg_i
    i_dec_idxs = Term.decreasingArgs i_fix
    full_args = map Var o_free_vars ++ o_args' ++ i_args
    
    unify_me = id
      . map (map ((+ succ (nlength o_free_bs)) . fst))
      -- ^ succ here accounts for the fix variable
      . filter (\xs -> length xs >= 2)
      . groupBy ((==) `on` snd)
      . sort
      $ o_dec_idxs' ++ i_dec_idxs'
      where
      o_dec_idxs' = id
        . map (\i -> (if i > enum arg_i then i - 1 else i, o_args !! i))
        $ filter (/= (enum arg_i)) o_dec_idxs
      i_dec_idxs' = id  
        . map (\i -> (i + (elength o_dec_idxs'), i_args !! i))
        $ i_dec_idxs
      
    o_args' = removeAt (enum arg_i) o_args
    

    old_ctx_t = id 
      . flip (foldr Term.unifyArgs) unify_me 
      . unflattenLam (i_fix_b : o_free_bs ++ o_fix_bs ++ i_fix_bs) 
      $ app o_fix' o_args'
      where
      o_fix' = id
        . Indices.liftMany (enum arg_c)
        . snd
        . flattenLam
        $ Term.abstractVars o_free_bs o_free_vars o_fix
        
      o_args' = id
        $ left_args ++ [i_term'] ++ right_args
        
      left_args =
        map (Var . enum . (+ i_arg_c)) [0..arg_i-1]
      right_args = 
        map (Var . enum . (+ i_arg_c)) [arg_i..o_arg_c-1]
      
      i_term' = App i_fix' i_args'
      i_args' = reverse (map (Var . enum) [0..i_arg_c - 1])
      i_fix' = Indices.liftMany (enum (arg_c + length o_free_bs)) (Var 0)
 
      o_arg_c  = length o_fix_bs
      i_arg_c = length i_fix_bs   
      arg_c = o_arg_c + i_arg_c
      
      o_fix_bs = removeAt (enum arg_i) (fst (flattenLam o_fix_t))
      i_fix_bs = fst (flattenLam i_fix_t)
      
fixfix _ = Fail.here


decreasingFreeVar :: Step m => Term -> m Term
decreasingFreeVar orig_t@(App fix@(Fix _ _ fix_t) args) = do
  Fusion.checkEnabled
  Fail.unless (length var_arg_is > 0)
  Fail.when (Term.beingFused fix)
  Fail.assert "dec-free-var given non lambda floated fixed-point"
    $ Term.isLambdaFloated fix
  
  App expr_fix@(Fix _ expr_b _) expr_args <- 
    Term.expressFreeVariables var_args fix
  
  Fail.assert ("expressFreeVariables failed in dec-free-var " 
              ++ "\n[expressing] " ++ show var_args
              ++ "\n[in] " ++ show fix
              ++ "\n[yielded] " ++ show expr_fix)
    $ expr_args == map Var var_args
    
  let (orig_bs, _) = flattenLam fix_t
      ctx_t = id
        . unflattenLam (expr_b:orig_bs)
        . app (Var (elength orig_bs))
        . map (Var . enum) 
        $ map (\v -> (length orig_bs - enum v) - 1) var_arg_is
        ++ reverse [0..length orig_bs - 1]
        
      full_t = Term.reduce ctx_t (expr_fix:args)
        
  Fail.assert "dec-free-var generated an incorrectly typed context"
    $ Type.get full_t == Type.get orig_t
  
  orig_s <- showM orig_t
  ctx_s <- showM ctx_t
  fix_s <- showM expr_fix
  
  new_fix <- id
    . History.check Name.FreeArgFusion full_t
    -- . trace ("\n\n[dec-free from] " ++ orig_s ++ "\n\n[context] " ++ ctx_s 
   --     ++ "\n\n[expressed fix] " ++ fix_s)
    $ fusion ctx_t expr_fix
    
  Transform.continue (app new_fix args)
  where
  -- The variable arguments we should attempt this technique on.
  -- They must be a decreasing argument, and free within the fixpoint itself. 
  var_arg_is :: [Nat]
  var_arg_is = id
    . sort
    . filter isFreeVar 
    $ Term.decreasingArgs fix
    where
    isFreeVar arg_i 
      | Var x <- args !! arg_i =
        x `Indices.freeWithin` fix
    isFreeVar _ = False
    
  var_args :: [Index]
  var_args = nubOrd (map (fromVar . (args !!)) var_arg_is) 
  
decreasingFreeVar _ = Fail.here


-- | If two or more decreasing arguments to a fixpoint are the same 
-- variable, we can sometimes fuse these arguments into one.
repeatedArg :: forall m . Step m => Term -> m Term
  
repeatedArg term@(App fix@(Fix _ fix_b fix_t) args) = do
  Fusion.checkEnabled
  Fail.unless (length args == length fix_bs)
  Fail.unless (Term.inductivelyTyped term)
  Fail.when (Term.beingFused fix)
  Fail.choose (map fuseRepeated rep_arg_is)
  where
  rep_arg_is :: [[Int]]
  rep_arg_is = id 
    . map (map enum)
    -- We only care about ones with at least a single repetition
    . filter ((> 1) . length)
    -- Group up all decreasing arguments which are equal
    . groupBy (((==) `on` Indices.free) `on` (args !!))
    -- We only care about variable arguments
    . filter (Term.isSimple . (args !!))
    $ Term.decreasingArgs fix
    
  (fix_bs, _) = flattenLam fix_t 
  
  fuseRepeated :: [Int] -> m Term
  fuseRepeated arg_is = do
    full_s <- showM full_t
    orig_s <- showM term
    ctx_s <- showM ctx_t
    new_fix <- id
      . tracE [("rep-arg orig", orig_s), ("rep-arg ctx", ctx_s), ("rep-arg full", full_s)]
      . History.check Name.RepArgFusion full_t
      $ fusion ctx_t fix
      
    Transform.continue (app new_fix args)
    where
    Just var_i = find (Term.isVar . (args !!)) arg_is
    Var arg_x = args !! var_i
    -- ^ One is guaranteed to be a variable 
    -- or this function will have been unfolded
    full_t = Term.reduce ctx_t [fix]
    
    ctx_t = id
      . Term.unifyArgs (map (succ . enum) arg_is)
      . unflattenLam (fix_b:fix_bs)
      . app (Var (elength fix_bs))
      . map (Var . enum)
      $ reverse (range fix_bs)

repeatedArg _ = Fail.here


matchFix :: forall m . Step m => Term -> m Term
matchFix term@(App fix@(Fix {}) xs)
  | Term.beingFused fix = Fail.here
  | not (any (isFix . leftmost) xs) =  do
    Fusion.checkEnabled
    cts <- Env.findConstraints usefulConstraint
    ctss <- showM cts
    let ct_set = Set.fromList cts
    Fail.when (null cts)
    
    term' <- id
      . Fusion.disable
      $ fuseConstraints cts term
    term'' <- id  
      . Direction.prover
      . Rewrite.run 
      $ Constraint.removeAll term'
    Fail.unless (term'' Quasi.< term)
    
    ts <- showM term
    ts' <- showM term''
    trace ("\n\n[match fix from] "
        ++ ts ++ "\n\n[context] " 
        ++ ctss ++ "\n\n[to] " ++ ts') 
        $ return term''
  where
  usefulConstraint :: Constraint -> Bool 
  usefulConstraint ct 
    | not (any (isFix . leftmost) ys)
    -- ^ Only applicable to matches on functions applied to variables
    , not (Set.null (Set.intersection (Set.fromList xs) (Set.fromList ys)))
    -- ^ Skip if no variables match
    , not (Type.isRecursive (matchInd ct)) =
      True
    where
    App (Fix {}) ys = matchedTerm ct
    
  usefulConstraint _ = False
  
  
  -- | Fuse all constraints in from left to right (foldl style), 
  -- ignoring failure and stopping if we reach
  -- a non fixed-point term, since we can only fuse into fixed-points
  fuseConstraints :: [Constraint] -> Term -> m Term
  fuseConstraints [] t = return t
  fuseConstraints _ t 
    | (not . isFix . leftmost) t = return t
  fuseConstraints (ct:cts) t@(App fix@(Fix {}) xs) = do
      mby_t' <- Fail.catch (fuseConstraint ct t)
      fuseConstraints cts (fromMaybe t mby_t')
      
      
  fuseConstraint :: Constraint -> Term -> m Term
  fuseConstraint ct oterm = do
    Fail.assert "fix-match pattern fix not lambda floated"
      $ Term.isLambdaFloated mfix
      
    Fail.assert "fix-match somehow generated a null set of matching variables" 
      $ length matched_is > 0
      
    m_s <- showM ct
    ctx_s <- showM (Term.reduce ctx_t (ofix:new_args))
    o_s <- showM oterm
    
    Type.assertEqM 
      ( "match-fix created an incorrectly typed context: " ++ ctx_s 
        ++ "\nmatch term: " ++ m_s 
        ++ "\ntarget term: " ++ o_s)
      (app ctx_t (ofix:new_args)) oterm
      
    fused_t <- id
      . History.check Name.MatchFixFusion full_t $ do
        new_fix <- fusion ctx_t ofix  
        let new_fix' 
             -- | isFix (leftmost new_fix) = Constraint.restrictFixDomain ct new_fix
              | otherwise = cleanupResult new_fix
        Fail.when (ofix Quasi.<= new_fix')
        return new_fix'
 
    return (Term.reduce fused_t new_args)
    where
    App mfix@(Fix _ _ mfix_t) margs = matchedTerm ct
    Con con : pargs = (flattenApp . matchedTo) ct
    App ofix@(Fix _ ofix_b ofix_t) oargs = oterm
    full_t = Term.reduce ctx_t [ofix]
    
    all_args = margs ++ oargs
    (m_bs, _) = flattenLam mfix_t
    (o_bs, _) = flattenLam ofix_t
    
    new_args = 
      removeAll (Set.fromList (map (enum . snd) matched_is)) all_args
      -- ^ Remove matching arguments
    
    -- The binding indices which match within the pattern 
    matched_is :: [(Nat, Nat)]
    matched_is = id
      . concatMap (\(i:is) -> map (\j -> (i, j)) is)
      . filter ((> 1) . length)
      . groupBy ((==) `on` (all_args !!))
      . sortBy (compare `on` (all_args !!))
      $ range all_args
      
    ctx_t = id
      . unflattenLam [ofix_b]
      . Term.equateArgsMany matched_is
      . unflattenLam (m_bs ++ o_bs)
      . Constraint.apply (Type.get term) ct'
      $ app fix_var ctx_oargs
      where
      toIdx :: Int -> Term
      toIdx i = Var (enum ((length all_args - i) - 1)) 
      
      fix_var = Var (elength all_args)
      ctx_margs = map toIdx [0..length margs-1]
      ctx_oargs = map toIdx [length margs..length all_args - 1]
      ct' = set matchTerm (app mfix' ctx_margs) ct
      mfix' = Indices.liftMany (nlength m_bs + nlength o_bs + 1) mfix
      
    -- Remove residual constraints on fixed-point results  
    cleanupResult = Constraint.removeWhen recFix
      where
      recFix ct _ = isFix (leftmost (matchedTerm ct))
        
matchFix _ = Fail.here
    

accumulation :: forall m . Step m => Term -> m Term
accumulation orig_t@(App fix@(Fix {}) args) = do
  Fusion.checkEnabled
  Fail.when (Term.beingFused fix)
  Fail.when (null acc_args)
  History.check Name.AccFusion orig_t $ do
    arg_tys <- mapM Type.getM args
    orig_s <- showM fix  
  --  tracE [("acc args of", orig_s), ("are", show acc_args)]
    id $ Fail.choose (map (tryArg arg_tys) acc_args)
  where
  acc_args = Term.accumulatingArgs fix
  
  tryArg :: [Type] -> Nat -> m Term
  tryArg arg_tys arg_n = do
    Fail.when (isVar (leftmost acc_arg))
    Fail.unless (isSimple acc_arg)
    ctx_s <- showM ctx_t
    orig_s <- showM orig_t
    new_fix <- fusion ctx_t fix
    return (app new_fix (removeAt arg_n args))
    where
    gen_tys = removeAt arg_n arg_tys
    gen_bs = zipWith (\i -> Bind ("X" ++ show i)) [0..] gen_tys
    fun_b = Bind "g" (Type.get fix)
    fun_var = Var (elength gen_bs) 
    
    acc_arg = Indices.liftMany (nlength gen_bs + 1) (args !! arg_n)
    arg_vars = id 
      . insertAt (enum arg_n) acc_arg 
      . reverse 
      . map (Var . enum)
      $ [0..length gen_bs - 1]
    
    ctx_t = unflattenLam (fun_b : gen_bs) (app fun_var arg_vars)
  
accumulation _ = Fail.here
  

discoverFold :: forall m . Step m => Term -> m Term
discoverFold orig_t@(App orig_fix@(Fix {}) orig_args) = id
  . History.check Name.FoldDiscovery orig_t
  . Env.forgetAllMatches $ do
    Fusion.checkEnabled
    Direction.requireInc
    Fail.unless (Set.size tags == 1)
    Fail.unless (Set.size to_calls == 1)
    Fail.unless (Type.has orig_t)
    Fail.unless (is_fixfix || is_other)
    -- ^ Time saving heuristic
    [(from_f, to_var)] <- Fusion.findTags tags
    args <- findArgs from_f
    let from_t = Term.reduce from_f args
    Fail.when (orig_t Quasi.<= from_t)
    from_s <- showM from_t   
    args_s <- showM args
    from_fs <- showM from_f
    orig_s <- showM orig_t
    to_s <- showM to_call
    fold_t <- id  
      . Fusion.forgetRewrites 
   --    . tracE [("discovering for", orig_s), ("aiming for", from_s), ("from_f", from_fs), ("args", args_s)] 
      $ findFold from_t
    let new_t = App fold_t [App (Var to_var) args]
    return new_t
  where                    
  tags = Set.delete Tag.omega (Tag.tags orig_t)
  tag = (head . Set.toList) tags
  orig_ty = Type.get orig_t
  
  is_fixfix = 
    any taggedFixCall orig_args
  is_other = 
    taggedFixCall orig_t
  
  taggedFixCall (App (Fix fix_i _ _) _) =
    get fixIndex fix_i == tag
  taggedFixCall _ = False
  
  to_calls = Term.collect taggedFixCall orig_t
  to_call@(App _ to_args) = (head . Set.toList) to_calls
  
  -- | Finds a context such that the original term is equal to the given
  -- term within this context
  findFold :: Term -> m Term
  findFold from_t = do
    Fail.unless (Type.has from_t)
    Fail.unless (Type.isInd from_ty)
    prop <- id
      . Env.bindMany c_bs
      . generaliseProp
      . Tag.map (const Tag.omega)
      . Leq orig_t' 
      $ Term.reduce fold_t (c_vars ++ [from_t'])
    prop_s <- Env.bindMany c_bs (showM prop)
    prop' <- id
      . Env.bindMany c_bs    
      . tracE [("discovery prop (before)", prop_s)]       
      . Memo.memo Name.FoldDiscovery prop 
      . Direction.prover
      $ Transform.continue prop
    prop_s' <- Env.bindMany c_bs (showM prop')
    unis <- id         
      . tracE [("discovery prop", prop_s')]                      
      . Env.trackOffsetT
      $ Fold.isoFoldM Term.branches solve prop'
    uni_s <- Env.bindMany c_bs $ showM (map Map.toList unis)
    uni <- joinUnifiers unis
    uni_s' <- Env.bindMany c_bs $ showM uni
    Fail.unless (Unifier.domain uni == c_var_set)
    
    hopefully_true <- id
      . Env.bindMany c_bs 
      . Direction.prover
      . Transform.continue
      . Unifier.apply uni 
      . ungeneraliseProp 
      $ prop'
    t_s <- Env.bindMany c_bs $ showM (Unifier.apply uni $ ungeneraliseProp prop')
    t_s' <- Env.bindMany c_bs (showM hopefully_true)
    tracE [("hopefully true (before)", t_s), ("hopefully true", t_s')]
      $ Fail.unless (hopefully_true == Term.truth)
    
    return  
      . Indices.lowerMany (nlength c_vars)
      . Unifier.apply uni
      $ Term.reduce fold_t c_vars
    where
    from_ty = Type.get from_t
    fold_t = Term.buildFold (Type.fromBase from_ty) orig_ty
    fold_tys = init (Type.argumentTypes (Type.get fold_t))
    c_bs = zipWith (\i ty -> Bind ("c" ++ show i) ty) [0..] fold_tys
    c_vars = (reverse . map (Var . enum)) [0..length c_bs - 1]
    c_var_set = Set.fromList (map fromVar c_vars)
    from_t' = liftHere from_t
    orig_t' = liftHere orig_t
      
    free_args
      | fix@(Fix {}) : args <- flattenApp from_t' = id
        . filter (\x -> Indices.freeWithin x fix)
        . map fromVar
        $ filter isVar args
      | otherwise = []
        
    generaliseProp t
      | is_fixfix = return (Term.tryGeneralise (liftHere to_call) t)
      | not (null free_args) = Term.tryGeneraliseInFix (head free_args) t
      | otherwise = return t
    ungeneraliseProp t
      | is_fixfix, isLam t = Term.reduce t [liftHere to_call]
      | not (null free_args), isLam t = Term.reduce t [Var (head free_args)]
      | otherwise = t
     
    liftHere = Indices.liftMany (nlength c_bs)
    
    solve :: Term -> Env.TrackOffsetT m [Unifier Term]
    solve p 
      | p == Term.falsity = Fail.here
      | p == Term.truth = return []
    solve (Leq x y) 
      | Just uni <- Unifier.find y x = do
        n <- Env.offset
        if not (Indices.lowerableBy n (Map.toList uni))
        then return []
        else do
          let uni' = Map.fromList (Indices.lowerMany n (Map.toList uni))
          if Unifier.domain uni' `Set.isSubsetOf` c_var_set
          then return [uni']
          else return []
    solve _ =
      return []
      
    joinUnifiers :: forall m . Fail.Can m 
      => [Unifier Term] -> m (Unifier Term)
    joinUnifiers unis = id
      . liftM Map.fromList 
      . collapse
      . groupByIndex
      $ concatMap Map.toList unis
      where
      groupByIndex :: [(Index, Term)] -> [(Index, [Term])]
      groupByIndex = id
        . map groupUp
        . groupBy ((==) `on` fst)
        . sort
        where
        groupUp cs@((i, t) : _) = 
          (i, map snd cs)
      
      collapse :: [(Index, [Term])] -> m [(Index, Term)]
      collapse css = mapM removeMaybe merged
        where
        has_default = True
          && not (Type.isRecursive (Type.fromBase from_ty))
          && any (isJust . snd) merged
        
        merged :: [(Index, Maybe Term)]
        merged = map (second merge) css
        
        merge :: [Term] -> Maybe Term
        merge [x] = return x
        merge xs = foldl1M m xs
          where
          m :: Term -> Term -> Maybe Term
          m x y =
            if x == y || y `Term.isSubterm` x
            then return x
            else if x `Term.isSubterm` y
            then return y
            else Fail.here

        removeMaybe :: (Index, Maybe Term) -> m (Index, Term)
        removeMaybe (c, Just x) = return (c, x)
        removeMaybe (c, Nothing) = do
          Fail.unless has_default
          return (c, orig_t')
          
    collapse _ = Fail.here
        
  -- | Find arguments to our rewrite term which make fold discovery
  -- applicable
  findArgs :: Term -> m [Term]
  findArgs from_f = do
    Fail.unless (isFix (leftmost from_t))
    -- ^ This technique doesn't work for match-fix fusion
    uni <- id
      . Unifier.unions
      . map fromJust
      . filter isJust
      . zipWith Unifier.find from_args 
      $ Indices.liftMany (nlength arg_bs) orig_args
    
    from_s <- showM from_f
    to_s <- showM orig_t
    uni_s <- Env.bindMany arg_bs $ showM (Map.toList uni)
    id
     -- . tracE [("find-args from", from_s), ("find-args to", to_s), ("unifier", uni_s)]
      $ Fail.unless (Unifier.domain uni == required_vars)  
      
    let uni_with_defaults = Map.union uni default_uni
        found_args = id
          . Indices.lowerMany (nlength arg_bs)
          . map (Unifier.apply uni_with_defaults)
          $ map Var arg_vars
    found_tys <- mapM Type.getM found_args
    Fail.unless (found_tys == arg_tys)
    args_s <- showM found_args
    id
     -- . tracE [("find-args args", args_s)]
      $ return found_args
    where
    (arg_bs, from_t) = flattenLam from_f
    from_calls = Term.collect taggedFixCall from_t
    App _ from_args = from_t
    arg_tys = map (get Type.bindType) arg_bs
    
    arg_vars :: [Index]
    arg_vars = id
      . map enum
      . reverse
      $ range arg_bs
 
    required_vars = 
      Set.intersection (Set.fromList arg_vars) (Indices.free from_t)
      
    default_uni = id
      . Map.fromList 
      . zip arg_vars 
      . map (\(Bind _ ty) -> Bot ty)
      $ arg_bs
      
      
discoverFold _ = Fail.here
    