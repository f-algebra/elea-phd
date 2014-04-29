-- | This module performs term simplifications which do 
-- not involve fixpoint fusion.
module Elea.Simplifier
(

)
where

import Elea.Prelude
import Elea.Term
import Elea.Show ( showM )
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Evaluation as Eval
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.Trans as Trans
{-

run :: Term -> Term
run = Fold.rewriteSteps steps

steps :: Fail.Can m => [Term -> m Term]
steps = Eval.steps ++
  [ const Fail.here
  , caseFun
  , constantFix
  , identityCase
  , finiteArgFix
  , unfoldFixInj
  , freeCaseFix
  , propagateMatch
  -- , finiteCaseFix   
  , unfoldWithinFix
  ]

  
-- | We do not want pattern matches to return function typed values,
-- so we add a new lambda above one if this is the case.
caseFun :: Fail.Can m => Term -> m Term
caseFun cse@(Case ind t alts) = do
  Fail.unless (length potential_bs > 0)
  return
    . Lam (head potential_bs)
    . Case ind (Indices.lift t)
    $ map appAlt alts
  where
  alt_ts = map (get altInner) alts
  
  potential_bs = mapMaybe findLam alts
    where
    findLam :: Alt -> Maybe Bind
    findLam (Alt _ (Lam b _)) = Just b
    findLam _ = Nothing
  
  appAlt (Alt bs alt_t) =
    Alt bs (app alt_t' [arg])
    where
    alt_t' = Indices.liftAt (toEnum (length bs)) alt_t
    arg = Var (toEnum (length bs))
    
caseFun _ = Fail.here


-- | If a fixpoint has a finite input to one of its decreasing arguments
-- then you can safely unfold it.
finiteArgFix :: Fail.Can m => Term -> m Term
finiteArgFix term@(App fix@(Fix {}) args)
  | any isFinite dec_args = 
    return (Eval.run (app (Term.unfoldFix fix) args))
  where
  dec_args = map (args `nth`) (Term.decreasingAppArgs term)
finiteArgFix _ = Fail.here


-- | Removes a pattern match which just returns the term it is matching upon.
identityCase :: Fail.Can m => Term -> m Term
identityCase (Case ind cse_t alts)
  | and (zipWith isIdAlt [0..] alts) = return cse_t
  where
  isIdAlt :: Nat -> Alt -> Bool
  isIdAlt n (Alt _ alt_t) = alt_t == altPattern ind n
identityCase _ = Fail.here



-- | Unfolds a 'Fix' if one of its decreasing arguments is a constructor term 
-- which is defined enough to fully reduce all matches on that argument.
unfoldFixInj :: Fail.Can m => Term -> m Term
unfoldFixInj term@(App fix@(Fix _ _ fix_t) args)
  | any isSafeConArg (Term.decreasingAppArgs term) = 
    return (Eval.run (app (Term.unfoldFix fix) args))
  where
  -- Check whether an argument is a constructor, and will reduce all pattern 
  -- matches upon it.
  isSafeConArg :: Int -> Bool
  isSafeConArg arg_i =
    isCon (leftmost arg) 
    && Set.null matched_rec_vars
    where
    arg = args !! arg_i
    rec_vars = recursiveConArgs arg
      
    matched_vars = id
      . Env.trackOffset
      . Env.liftTracked
      $ Fold.foldM matchedVars unwrapped_t
      
    matched_rec_vars = Set.intersection matched_vars rec_vars
    
    unwrapped_t = id
      . Eval.run 
      . App fix_t 
      . setAt arg_i (Indices.lift arg)
      $ replicate (length args) (Var 0)
      
    recursiveConArgs :: Term -> Set Index
    recursiveConArgs (Var x) = Set.singleton x
    recursiveConArgs (flattenApp -> Con ind n : con_args) = id
      . concatMap recursiveConArgs
      . map (con_args !!)
      $ Type.recursiveArgs ind n
    recursiveConArgs _ = mempty
    
    matchedVars :: Term -> Env.TrackOffset (Set Index) 
    matchedVars (Case _ (Var x) _) = do
      can_lower <- Env.lowerableByOffset x
      if can_lower
      then liftM Set.singleton (Env.lowerByOffset x)
      else return mempty
    matchedVars _ = 
      return mempty
    
unfoldFixInj _ = Fail.here


-- | If we pattern match inside a 'Fix', but only using variables that exist
-- outside of the 'Fix', then we can float this pattern match outside
-- of the 'Fix'.
freeCaseFix :: Fail.Can m => Term -> m Term
freeCaseFix fix@(Fix _ _ fix_t) = do
  free_case <- id
    . Fail.fromMaybe
    . Env.trackOffset
    . Env.liftTracked
    $ Fold.findM freeCases fix_t
  return (Term.applyCase free_case fix)
  where
  freeCases :: Term -> Env.TrackOffset (Maybe Term)
  freeCases cse@(Case _ cse_t _) = do
    idx_offset <- Env.tracked
    if any (< idx_offset) (Indices.free cse_t) 
    then return Nothing
    else return 
       . Just
       . Indices.lowerMany (enum idx_offset) 
       $ cse
  freeCases _ = 
    return Nothing
freeCaseFix _ = Fail.here


-- | Replace all instances of a term with the pattern it is matched to.
propagateMatch :: Fail.Can m => Term -> m Term
propagateMatch (Case ind cse_t alts) 
  | any altSubterm alts =
    return (Case ind cse_t (zipWith propagateAlt [0..] alts))
  where
  altSubterm (Alt bs alt_t) = 
    Term.isSubterm (Indices.liftMany (length bs) cse_t) alt_t
  
  propagateAlt :: Nat -> Alt -> Alt
  propagateAlt n (Alt bs alt_t) = id
    . Alt bs  
    . Eval.run
    $ Term.replace cse_t' alt_p alt_t
    where
    alt_p = Term.altPattern ind n
    cse_t' = Indices.liftMany (length bs) cse_t
    
propagateMatch _ = Fail.here


-- | Unfolds a 'Fix' which is being pattern matched upon if that pattern
-- match only uses a finite amount of information from the 'Fix'.
-- Currently only works for a single unrolling, but otherwise we'd need an 
-- arbitrary amount of unrolling, which seems difficult.
finiteCaseFix :: Fail.Can m => Term -> m Term
finiteCaseFix term@(Case ind (App (Fix _ _ fix_t) args) alts) = do
  -- This line is just for speed.
  -- It's just a guess, but I can't think of a non-recursive datatype
  -- returning function which this would work on.
  Fail.unless (Type.isRecursive ind)
  Fail.unless (and (zipWith finiteAlt [0..] alts))
  
  -- Check that unrolling the function removed recursive calls 
  Fail.when (0 `Indices.freeWithin` unrolled)
  return (Indices.lower unrolled)
  where
  extendedEval = Fold.rewriteSteps (Eval.steps ++ [finiteCaseFix])
  unrolled = id
    . extendedEval
    . Case ind (App fix_t (Indices.lift args)) 
    $ Indices.lift alts
    
  -- A branch in which a recursive pattern variable is used
  finiteAlt :: Nat -> Alt -> Bool
  finiteAlt alt_n (Alt bs alt_t) =
    Set.null (Indices.free alt_t `Set.intersection` rec_vars)
    where
    rec_vars = Set.fromList (Type.recursiveArgIndices ind alt_n)
  
finiteCaseFix _ = Fail.here


-- | If a recursive function just returns the same value, regardless of its
-- inputs, just reduce it to that value.
constantFix :: forall m . Fail.Can m => Term -> m Term
constantFix (Fix _ fix_b fix_t)
  | Just [result] <- mby_results = guessConstant result
  | Just [] <- mby_results = guessConstant (Unr result_ty)
  where
  (arg_bs, _) = flattenLam fix_t
  result_ty = Type.returnType (get Type.boundType fix_b)
  mby_results = potentialResults fix_t

  potentialResults :: Term -> Maybe [Term]
  potentialResults = id
    . map toList
    . Env.trackIndices 0
    . runMaybeT
    . Fold.isoFoldM Term.branches resultTerm
    where
    resultTerm :: Term -> MaybeT (Env.TrackIndices Index) (Set Term)
    resultTerm (Unr _) = return mempty
    resultTerm term = do
      fix_f <- Env.tracked
      if leftmost term == Var fix_f
      then return mempty
      else do
        let depth = enum fix_f
        -- Checking we can lower by one extra makes sure there is no
        -- occurrence of the 'fix'ed variable itself.
        guard (Indices.lowerableBy (depth + 1) term)
        return
          . Set.singleton
          $ Indices.lowerMany depth term
        
  guessConstant :: Term -> m Term
  guessConstant guess_t 
    | Just [] <- mby_results' = return const_t
    | Just [guess_t'] <- mby_results' = id
      . assert (guess_t == guess_t') 
      $ return const_t
    where
    rec_f = id
      . unflattenLam arg_bs
      . Indices.liftMany (length arg_bs)
      $ guess_t
      
    fix_t' = Eval.run (Term.replace (Var 0) rec_f fix_t)
    mby_results' = potentialResults fix_t'
    const_t = Indices.lower rec_f
    
  guessConstant _ = 
    Fail.here
        
constantFix _ = 
  Fail.here
  
  
-- | Unfolds a 'Fix' within itself if it can be unrolled at
-- at a point it is called recursively.
unfoldWithinFix :: Fail.Can m => Term -> m Term
unfoldWithinFix fix@(Fix fix_i fix_b fix_t) = do
  Fail.unless any_replaced
  return (Fix fix_i fix_b fix_t')
  where
  (fix_t', Monoid.Any any_replaced) = id
    . Env.trackIndices (0, fix_t)
    . runWriterT
    $ Fold.transformM unfold fix_t
  
  unfold :: Term -> WriterT Monoid.Any (Env.TrackIndices (Index, Term)) Term
  unfold term@(App (Var f) args) = do
    (fix_f, fix_t) <- Env.tracked
    if f /= fix_f || any (not . Term.isFinite) args
    then return term
    else do
      tell (Any True)
      (_, fix_t) <- Env.tracked
      return (Eval.run (app fix_t args))
  unfold other = 
    return other
  
unfoldWithinFix _ = Fail.here
-}
