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
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

run :: (Defs.Read m, Env.Read m) => Term -> m Term
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
fixfix :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) => 
  Term -> m Term

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

  -- The internal simplification used in fixfix fusion.
  -- Recursively runs fusion, and then attempts to extract fixpoints
  -- using invention.
  simplify :: Context -> Index -> Term -> m Term
  simplify ctx fix_f term = do
    term' <- run term
    Env.alsoTrack 0
      $ Fold.transformM extract term'
    where
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
        fix_f' <- Env.liftByOffset fix_f
        guard (fix_f' == f)
        
        -- If a unifier exists, then we do not need to do fixpoint extraction
        orig_term <- Env.liftByOffset (Context.apply ctx (Var fix_f))
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
              
        f_ty <- Type.get f_term
                                                                       
        invented_ctx <- id
          . Env.bind (Bind "extrX" f_ty)
          $ Fix.invention Simp.run orig_term' term'
        
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
repeatedArg :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) => 
  Term -> m Term
  
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
  simplify :: Context -> Index -> Term -> m Term
  simplify _ _ = Simp.run

repeatedArg _ = Fail.here


-- | Match-Fix fusion. Makes use of an environment which you can read pattern 
-- matches from.
matchFix :: forall m . (Env.MatchRead m, Fail.Can m, Defs.Read m) => 
  Term -> m Term
  
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
  simplify :: Context -> Index -> Term -> m Term
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
      -- then we can apply fusion to the context constraining @match_t@ to
      -- have the constructor at @con_n@
      context <- Term.constraint match_t con_n result_ty
      -- Appending contexts composes them in the natural way
      Fix.fusion simplify (context ++ args_ctx) fix'
    where
    cons = Type.unfold ind
    match_fix_info = fixInfo (leftmost match_t)
    args_ctx = Context.make (\t -> app t args)
    
    -- The new fused matches are all fused matches of the original fix,
    -- those of the match we are fusing in, and the match itself.
    -- 'FixInfo' is a monoid, so we can use append here.
    fix_info' = fix_info ++ match_fix_info ++ (FixInfo [(match_t, con_n)])
    fix' = Fix fix_info' fix_b fix_t
        
matchFix _ = Fail.here


