-- | Some term transformation steps that rely on fixpoint fusion.
module Elea.Transform.Fusion
(
  run
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
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
import qualified Elea.Monad.Rewrite as Rewrite
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Poset as Quasi

-- TODO match fix needs global memoisation of (Set Constraint, Term) pairs,
-- since the rising (==) check in the traverse rules is wiping
-- the failed constraint memory. It's also just faster.


type Env m = 
  ( Defs.Read m
  , Env.All m
  , Discovery.Tells m
  , Tag.Gen m
  , History.Env m 
  , Rewrite.Env m
  , Memo.Can m )
  
  
type Step m =
  ( Rewrite.Step m
  , Env m )
 
  
run :: Env m => Term -> m Term
run = Transform.fix (Transform.compose all_steps)
  where
  all_steps = []
    ++ Eval.transformSteps
    ++ Rewrite.rewriteSteps
    ++ Eval.traverseSteps
    ++ Rewrite.expressSteps
    -- ^ Prioritising rewrites over descending into terms
    -- speeds things up a bit
    ++ Simp.steps
    ++ Prover.steps
    ++ steps
    
  
steps :: Step m => [Term -> m Term]
steps = 
  [ const Fail.here
  , fixfix
  , decreasingFreeVar
  , repeatedArg
 -- , matchFix
  ]


fusion :: Step m => Term -> Term -> m Term
fusion ctx_t fix@(Fix fix_i fix_b fix_t) = id
  . Env.forgetAllMatches 
  . Memo.memo Name.Fusion orig_t $ do
    Fail.assert "trying to fuse the unfusable"
      $ not (Term.beingFused fix)
  
    t_s <- showM orig_t
  
    temp_idx <- Tag.make
    let temp_i = set fixIndex temp_idx fix_i
        temp_fix = Fix temp_i fix_b fix_t
        rewrite_from = id 
          . Simp.run
        --  . traceMe "\n\n!![rewrite_from]"
          . Indices.lift 
          $ app ctx_t [temp_fix]
  
    Type.assertEqM "[fixfix]" (Indices.lower rewrite_from) orig_t
           
    t_s' <- Env.bind new_fix_b $ showM rewrite_from
   -- t_s'' <- showM (Eval.run (app ctx_t [Term.unfoldFix temp_fix]))
    
    new_fix_t <- id  
      . Env.bind new_fix_b
      . Rewrite.local temp_idx rewrite_from 0
      . Transform.continue
      . trace ("\n\n[fusing <" ++ show temp_idx   ++ ">] " ++ t_s)
     -- . trace ("\n\n[replacing] " ++ t_s')
      -- . trace ("\n\n[transforming< " ++ show temp_idx ++ ">] " ++ t_s'')
      . Indices.lift
      -- ^ Make room for our new variables we are rewriting to
      $ app ctx_t [Term.unfoldFix temp_fix]
      
    t_s''' <- showM (Eval.run (Fix fix_i new_fix_b new_fix_t))
      
    if not (0 `Indices.freeWithin` new_fix_t) 
    then do 
      when (Set.member temp_idx (Tag.tags new_fix_t)) $ do
        trace ("\n\n[failing <" ++ show temp_idx ++ ">] " ++ t_s''')
          Fail.here
      return 
        . Simp.run
        $ Indices.lower new_fix_t
    else return
      . Simp.run
      . trace ("\n\n[yielding <" ++ show temp_idx ++ ">] " ++ t_s''') 
      . Fix fix_i new_fix_b 
      $ Tag.replace temp_idx orig_idx new_fix_t
  where
  orig_t = Term.reduce ctx_t [fix]
  orig_idx = get fixIndex fix_i
  
  new_fix_b = set Type.bindType (Type.get orig_t) fix_b
  
    
fixfix :: forall m . Step m => Term -> m Term
fixfix o_term@(App o_fix@(Fix fix_i _ o_fix_t) o_args) = do
  -- ^ o_ is outer, i_ is inner
  
  Fail.assert ("fixfix given non lambda floated outer fixed-point" 
              ++ show o_fix ++ "\n" ++ show (Type.argumentTypes (Type.get o_fix))
                ++ ", " ++ show (fst (flattenLam o_fix_t)))
    $ Term.isLambdaFloated o_fix
    
  -- Pick the first one which does not fail
  Fail.choose
    -- Run fixfixArg on every decreasing fixpoint argument position
    . map (fixArg . enum)
    . filter (Term.isFix . Term.leftmost . (o_args !!))
    $ Term.decreasingArgs o_fix 
  where  
  fixArg :: Int -> m Term
  -- Run fix-fix fusion on the argument at the given index
  fixArg arg_i = do
    Fail.when (Term.beingFused i_fix)
    Fail.assert ("fixfix given non lambda floated inner fixed-point " 
                ++ show i_fix)
      $ Term.isLambdaFloated i_fix
    Type.assertEqM "fix-fix created an incorrectly typed context" o_term full_t

    History.check Name.FixFixFusion gen_t $ do
      new_fix <- fusion ctx_t i_fix
      let new_term = app new_fix (o_args' ++ i_args)
      new_term' <- Transform.continue new_term
      ts <- showM new_term'
      trace ("\n\n[fixfix yielded]\n" ++ ts)
        $ return new_term'
    where
    i_term@(App i_fix@(Fix _ i_fix_b i_fix_t) i_args) = o_args !! arg_i
    o_args' = removeAt (enum arg_i) o_args
    
    gen_t = Eval.run (app ctx_t [i_fix])
    full_t = app ctx_t (i_fix:(o_args' ++ i_args))
   
    ctx_t = id 
      . unflattenLam (i_fix_b : o_fix_bs ++ i_fix_bs) 
      $ app o_fix' o_args'
      where
      o_fix' = Indices.liftMany (enum (o_arg_c + i_arg_c + 1)) o_fix
      o_args' = id
        $ left_args ++ [i_term'] ++ right_args
        
      left_args =
        map (Var . enum . (+ i_arg_c)) [0..arg_i-1]
      right_args = 
        map (Var . enum . (+ i_arg_c)) [arg_i..o_arg_c-1]
      
      i_term' = App i_fix' i_args'
      i_args' = reverse (map (Var . enum) [0..i_arg_c - 1])
      i_fix' = Indices.liftMany (enum arg_c) (Var 0)
             
      o_fix_bs = removeAt (enum arg_i) (fst (flattenLam o_fix_t))
      i_fix_bs = fst (flattenLam i_fix_t)
        
      o_arg_c  = length o_fix_bs
      i_arg_c = length i_fix_bs   
      arg_c = o_arg_c + i_arg_c
      
fixfix _ = Fail.here


decreasingFreeVar :: Step m => Term -> m Term
decreasingFreeVar orig_t@(App fix@(Fix _ _ fix_t) args) = do
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
    . groupBy ((==) `on` (args !!))
    -- We only care about variable arguments
    . filter (Term.isVar . (args !!))
    $ Term.decreasingArgs fix
    
  (fix_bs, _) = flattenLam fix_t
  
  fuseRepeated :: [Int] -> m Term
  fuseRepeated arg_is = do
    full_s <- showM full_t
    
    new_fix <- id
      . trace ("\n\n[rep-arg] " ++ full_s)
      . History.check Name.RepArgFusion full_t
      $ fusion ctx_t fix
      
    Transform.continue (app new_fix args')
    where
    arg_set :: Set Nat
    arg_set = Set.fromList (map enum arg_is)
    
    full_t = Term.reduce ctx_t [fix]
    args' = (args !! head arg_is) : removeAll arg_set args
    
    ctx_t = id
      . unflattenLam (fix_b:ctx_bs)
      . app (Var (elength ctx_bs))
      $ reverse ctx_args
      where
      non_arg_is = removeAll arg_set [0..length args - 1]
      
      arg_b = fix_bs !! ((length args - head arg_is) - 1)
      ctx_bs = arg_b : removeAll arg_set fix_bs
      ctx_args = map getArg (range args)
        where
        getArg :: Nat -> Term
        getArg n 
          | enum n `elem` arg_is = Var (enum (length ctx_bs - 1))
          | otherwise = id
              . Var
              . enum
              . fromJust
              $ findIndex (== enum n) non_arg_is

repeatedArg _ = Fail.here


matchFix :: forall m . Step m => Term -> m Term
matchFix term@(App fix@(Fix {}) xs)
  | Term.beingFused fix = Fail.here
  | not (any (isFix . leftmost) xs) =  do
    cts <- Env.findConstraints usefulConstraint
    ctss <- showM cts
    let ct_set = Set.fromList cts
    Fail.when (null cts)
    
    term' <- fuseConstraints cts term
    Fail.unless (term' Quasi.< term)
    
    ts <- showM term
    ts' <- showM term'
    
    trace ("\n\n[match fix from] "
        ++ ts ++ "\n\n[context] " 
        ++ ctss ++ "\n\n[to] " ++ ts') $ 
      return term'
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
    
   -- trace ("\n\n[match-fix original] " ++ o_s 
    --  ++ "\n\n[match] " ++ m_s 
    --  ++ "\n\n[context] " ++ ctx_s)
    --  (return ())
      
    fused_t <- History.check Name.MatchFixFusion full_t $ do
      new_fix <- fusion ctx_t ofix  
      let new_fix' 
           -- | isFix (leftmost new_fix) = Constraint.restrictFixDomain ct new_fix
            | otherwise = cleanupResult new_fix
      return new_fix'
 
    return
      . Simp.run
      $ app fused_t new_args
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
      ct' = set matchTerm (app mfix ctx_margs) ct
      
    -- Remove residual constraints on fixed-point results  
    cleanupResult = Constraint.removeWhen recFix
      where
      recFix ct _ = isFix (leftmost (matchedTerm ct))
        
    
matchFix _ = Fail.here
    


{-
  

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
        App inner_f' args' = Indices.liftMany (nlength alt_bs) inner_t
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
          -}
