-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Term.Ext
(
  module Elea.Term,
  branches,
  recursionScheme,
  replace,
  replaceAll,
  unfoldFix,
  unwrapFix,
  collectM, collect,
  decreasingArgs,
  decreasingAppArgs,
  constantArgs,
  unusedArgs,
  accumulatingArgs,
  applyCase,
  applyCases,
  reduce,
  generaliseArgs,
  generaliseTerms,
  generaliseUninterpreted,
  tuple,
  isFiniteMatch,
  expressFreeVariable,
  expressFreeVariables,
  revertMatches,
  commuteMatchesWhenM,
  occurrences,
  isSubterm,
  removeSubterms,
  freeSubtermsOf,
  freeVars,
  floatRecCallInwards,
  isLambdaFloated,
  findArguments,
  findConstrainedArgs,
  abstractVar,
  abstractVars,
  mapFixInfo,
  equateArgs,
  equateArgsMany,
  strictWithin,
  strictAcross,
  strictArgs,
  tryGeneralise,
  tryGeneraliseInFix,
  matchedWithin,
  unifyArgs,
  buildContext,
  buildCase,
  isProductive
)
where

import Elea.Prelude hiding ( replace )
import Elea.Term
import qualified Elea.Type.Ext as Type
import qualified Elea.Term.Index as Indices
import qualified Elea.Term.Constraint as Constraint
import qualified Elea.Monad.Env as Env
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Monad.Fusion as Fusion
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

-- | A wrapper around 'Term' for the 'branchesOnly' isomorphism.
newtype BranchesOnly = BranchesOnly { notBranchesOnly :: Term }
  deriving ( Eq, Ord, Show )

-- | A 'Term' isomorphism whose 'Fold.Transformable' instance 
-- only runs on the innermost terms of pattern matches and lambda abstractions.
-- We can use this isomorphism for the /iso/ style functions in 'Elea.Foldable'.
-- For example, to call @Fold.all p@ over just branches we use
-- > Fold.isoAll branchesOnly p t
-- > where p :: Term -> Bool, t :: Term
branches :: Fold.Iso Term BranchesOnly
branches = Fold.iso BranchesOnly notBranchesOnly

type instance Fold.Base BranchesOnly = Term'
  
instance Fold.Foldable BranchesOnly where
  project = fmap BranchesOnly . Fold.project . notBranchesOnly
  
instance Fold.Unfoldable BranchesOnly where
  embed = BranchesOnly . Fold.embed . fmap notBranchesOnly
  
instance Env.Write m => Fold.FoldableM m BranchesOnly where
  distM = Fold.distM . fmap (second notBranchesOnly)

instance Env.Write m => Fold.TransformableM m BranchesOnly where
  transformM f = id
    . liftM BranchesOnly 
    . Fold.selectiveTransformM (return . branches) f'
    . notBranchesOnly
    where
    f' = liftM notBranchesOnly . f . BranchesOnly
    
    branches :: Term -> (Bool, Term' Bool)
    branches (Case cse_t alts) =
      (False, Case' False (map descAlt alts))
      where 
      descAlt (Alt con bs alt_t) = Alt' con bs True
    branches (Lam b t) = 
      (False, Lam' b True)
    branches term = 
      (True, fmap (const False) (Fold.project term))
      
      
-- | A wrapper around 'Term' for the 'recursionScheme' isomorphism.
newtype RecursionScheme = RecursionScheme { notRecursionScheme :: Term }
  deriving ( Eq, Ord, Show )

-- | A 'Term' isomorphism whose 'Fold.Transformable' instance 
-- only runs on the pattern matches and nested pattern matches, until
-- we reach the end of a branch, when it stops.
recursionScheme :: Fold.Iso Term RecursionScheme
recursionScheme = Fold.iso RecursionScheme notRecursionScheme

type instance Fold.Base RecursionScheme = Term'
  
instance Fold.Foldable RecursionScheme where
  project = fmap RecursionScheme . Fold.project . notRecursionScheme
  
instance Fold.Unfoldable RecursionScheme where
  embed = RecursionScheme . Fold.embed . fmap notRecursionScheme
  
instance Env.Write m => Fold.FoldableM m RecursionScheme where
  distM = Fold.distM . fmap (second notRecursionScheme)

instance Env.Write m => Fold.TransformableM m RecursionScheme where
  transformM f = id
    . liftM RecursionScheme 
    . Fold.selectiveTransformM (return . scheme) f'
    . notRecursionScheme
    where
    f' = liftM notRecursionScheme . f . RecursionScheme
    
    scheme :: Term -> (Bool, Term' Bool)
    scheme (Case cse_t alts) =
      (True, Case' False (map descAlt alts))
      where 
      descAlt (Alt con bs alt_t) = 
        Alt' con bs True
    scheme (Lam b t) =
      (False, Lam' b True)
    scheme term = 
      (False, fmap (const False) (Fold.project term))
      
      
unfoldFix :: Term -> Term
unfoldFix fix@(Fix _ _ fix_t) = 
  Indices.subst fix fix_t
  
  
-- | Unfolds a fixpoint a given number of times and replace the fix variable
-- with 'Unr'eachable.
unwrapFix :: Nat -> Term -> Term
unwrapFix 0 fix@(Fix {}) = Bot (Type.get fix)
unwrapFix n fix@(Fix _ _ fix_t) =
  Indices.subst (unwrapFix (n - 1) fix) fix_t
  
  
-- | Collect terms which fulfil a given predicate. 
-- The variables of these terms must be free outside the original term,
-- and will be automatically lowered to the correct indices.
collectM :: forall m . Env.Write m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    condition <- (Trans.lift . Trans.lift . p) t
    Fail.unless condition
    
    lowerable <- Env.lowerableByOffset t
    Fail.unless lowerable
    
    Env.lowerByOffset t
    
-- | See 'collectM'
collect :: (Term -> Bool) -> Term -> Set Term
collect p = runIdentity . collectM (Identity . p)


-- | The variables or uninterpreted function application terms
-- whose value must be known in order to evaluate the given term.
strictWithin :: Term -> Set Term
strictWithin term
  | (isVar . leftmost) term = Set.singleton term
strictWithin (Case cse_t alts) = 
  Set.union (strictWithin cse_t) (strictAcross alts)
strictWithin (App fix@(Fix {}) args) = 
  Set.unions (map strictWithin strict_args)
  where
  strict_args = map (args !!) (strictArgs fix)
strictWithin (Leq x y) = 
  Set.union (strictWithin x) (strictWithin y)
strictWithin _ = Set.empty


-- | Terms which are strict in every supplied branch
strictAcross :: [Alt] -> Set Term
strictAcross alts = id
  . fromMaybe Set.empty
  . foldl1 merge
  $ map withinAlt alts
  where
  merge :: Maybe (Set Term) -> Maybe (Set Term) -> Maybe (Set Term)
  merge Nothing ys = ys
  merge xs Nothing = xs
  merge (Just xs) (Just ys) = 
    Just (Set.intersection xs ys)
  
  -- 'Nothing' means the universe of all terms
  -- so if an Alt returns _|_ then it is strict in every term
  withinAlt :: Alt -> Maybe (Set Term)
  withinAlt (Alt c bs (Bot _)) = Nothing
  withinAlt (Alt c bs alt_t) = id
    . Just
    . Set.mapMonotonic (Indices.lowerMany (nlength bs))
    . Set.filter (Indices.lowerableBy (nlength bs))
    $ strictWithin alt_t


-- | Return the argument indices which are strict for the given fixed-point
strictArgs :: Term -> [Nat]
strictArgs (Fix _ _ fix_t) = 
  map toArgPos strict_vars
  where
  (arg_bs, fix_body) = flattenLam fix_t
  
  -- Take the index of the strict argument in the term and convert it into
  -- the integer position index of that argument
  toArgPos :: Index -> Nat
  toArgPos idx = enum ((length arg_bs - enum idx) - 1)
  
  strict_vars :: [Index]
  strict_vars = id
    . map fromVar
    . filter isVar
    . Set.toList
    $ strictWithin fix_body
    

-- | Replace all instances of one term with another within a term.
replace :: Term -> Term -> Term -> Term
replace me with = replaceAll [(me, with)]

replaceAll :: [(Term, Term)] -> Term -> Term
replaceAll repls = id
  . Env.trackIndices repls
  . Fold.transformM doReplace
  where
  doReplace :: Term -> Env.TrackIndices [(Term, Term)] Term
  doReplace term = do
    repls <- Env.tracked
    case find ((== term) . fst) repls of
      Nothing -> return term
      Just (_, term') -> return term'

-- | A wrapped around 'decreasingArgs' which takes a fixpoint with arguments
-- applied and removes any return indices which are greater than the length
-- of the arguments.
-- Helps stop 'decreasingArgs' causing errors on partially applied 
-- fixpoints.
decreasingAppArgs :: Term -> [Nat]
decreasingAppArgs (App fix args) = 
  filter (nlength args >) (decreasingArgs fix)
  
  
-- | Returns the indices of the strictly decreasing arguments for
-- a given function. Undefined if not given a 'Fix'.
decreasingArgs :: Term -> [Nat]
decreasingArgs fix@(Fix _ fix_b fix_t) = 
  Set.toList (Set.fromList dec_or_const_args Set.\\ Set.fromList const_args)
  where
  (arg_bs, fix_body) = flattenLam fix_t
  dec_or_const_args = filter isDecreasing (range arg_bs)
  const_args = constantArgs fix

  isDecreasing :: Nat -> Bool
  isDecreasing arg_i = id
    . Env.trackOffset
    
    -- We track all terms which are 
    -- structurally smaller than our starting argument
    . Env.trackSmallerThan arg_var
    $ Fold.allM decreasing fix_body
    where
    -- The deBrujin index of the lambda bound variable we are tracking
    arg_var = Var (enum (length arg_bs - (enum arg_i + 1))) (arg_bs !! arg_i)
    
    -- The deBrujin index of the fix bound function variable
    fix_var :: Index = elength arg_bs
    
    decreasing :: 
      Term -> Env.TrackSmallerTermsT (Env.TrackIndices Index) Bool
    decreasing t@(App (Var f _) args) = do
      fix_var' <- Trans.lift (Env.liftByOffset fix_var)
      arg_var' <- Trans.lift (Env.liftByOffset arg_var)
      if fix_var' /= f || arg_i >= nlength args
      then return True
      else if arg_var' == (args !! arg_i)
      then return True
      else Env.isSmaller (args !! arg_i)
    decreasing _ = 
      return True
  
      
unusedArgs :: Term -> [Nat]
unusedArgs fix@(Fix _ _ fix_t) =
  filter isUnused (range arg_bs)
  where
  (arg_bs, body_t) = flattenLam fix_t
      
  isUnused :: Nat -> Bool
  isUnused n = not (idx `Indices.freeWithin` body_t) 
    where
    idx :: Index = enum ((nlength arg_bs - 1) - n)
  
  
constantArgs :: Term -> [Nat]
constantArgs (Fix _ _ fix_t) = 
  filter isConstArg (range arg_bs)
  where
  (arg_bs, fix_body) = flattenLam fix_t
  arg_count = length arg_bs
  fix_f = enum arg_count :: Index
  
  isConstArg :: Nat -> Bool
  isConstArg arg_i = id
    . not
    . Env.trackIndices (fix_f, Var arg_x (arg_bs !! arg_i))
    $ Fold.anyM isntConst fix_body
    where
    -- The index of the argument we are tracking as it was bound
    -- by the lambdas of the function
    arg_x = enum (nlength arg_bs - (arg_i + 1))
    
    -- Whether this given argument changes at a recursive call site
    isntConst :: Term -> Env.TrackIndices (Index, Term) Bool
    isntConst (App (Var f _) args) = do
      (fix_f, arg_t) <- Env.tracked
      return 
        $ fix_f == f
        -- If the number of arguments differs then this function is not
        -- in the correct shape for this process. So we fail by saying
        -- the argument changed.
        && (length args /= arg_count
        || arg_t /= (args !! arg_i))
    isntConst _ = 
      return False

      
accumulatingArgs :: Term -> [Nat]
accumulatingArgs fix@(Fix _ _ fix_t) 
  | acc_idxss == [] = []
  | otherwise = id
    . Set.toList
    $ foldl1 Set.intersection acc_idxss
  where
  (arg_bs, body_t) = flattenLam fix_t
  fix_var :: Index = (enum . length) arg_bs 
  arg_vars = zipWith (Var . enum) (reverse (range arg_bs)) arg_bs
    
  acc_idxss :: [Set Nat]
  acc_idxss = id
    . Env.trackOffset 
    $ Fold.foldM accumulates body_t
  
  accumulates :: Term -> Env.TrackOffset [Set Nat]
  accumulates (App (Var f _) xs) = do
    fix_var' <- Env.liftByOffset fix_var 
    if fix_var' /= f || length xs /= length arg_vars
    then return []
    else do
      arg_vars' <- Env.liftByOffset arg_vars 
      let isAcc i = (arg_vars' !! i) `isStrictSubterm` (xs !! i)
      return 
        . (return :: a -> [a])
        . Set.fromList
        . map enum
        . findIndices isAcc 
        $ range xs
  accumulates _ = 
    return []
    
      
-- | Take a case-of term and replace the result term down each branch
-- with the second term argument.
applyCase :: Term -> Term -> Term
applyCase (Case cse_t alts) inner_t = 
  Case cse_t (map mkAlt alts)
  where
  mkAlt :: Alt -> Alt
  mkAlt (Alt con bs _) = 
    Alt con bs (Indices.liftMany (nlength bs) inner_t)
    
applyCases :: [Term] -> Term -> Term
applyCases cs t = foldr applyCase t cs  


reduce :: Term -> [Term] -> Term
reduce (Lam _ rhs) (x:xs) = 
  reduce (Indices.subst x rhs) xs
reduce (Bot (Type.Fun _ res_ty)) (x:xs) =
  reduce (Bot res_ty) xs
reduce f xs = 
  app f xs    
 
  
-- | Generalise all the arguments of a term to fresh variables.
-- The first argument of the inner computation to run will lift 
-- indices by the number of new variables.
generaliseArgs :: forall m a .
    (Env.Read m, Defs.Read m, Substitutable a, Inner a ~ Term) =>
  Term -> (Indices.Shift -> Term -> m a) -> m a
generaliseArgs (App func args) run = do
  -- Use the type of every arguments to generate bindings for our new 
  -- generalised variables.
  gen_bs <- mapM makeBind (range args) 
  let new_vars = zipWith (Var . enum) [0..length args - 1] gen_bs
        
  -- Run the inner computation
  done_t <- id
    . Env.bindMany (reverse gen_bs)
    $ run liftHere (App (liftHere func) new_vars)
    
  -- Reverse the generalisation
  return
    . foldr Indices.subst done_t
    . zipWith Indices.liftMany [0..]
    $ reverse args
  where
  makeBind :: Nat -> m Bind
  makeBind n
    | Var x _ <- args !! n = Env.boundAt x
  makeBind n = do
    ty <- Type.getM (args !! n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (nlength args)
  
  
-- | Like 'generaliseArgs' but generalises /every/ occurrence of a set of terms
-- within another.
generaliseTerms :: forall m a t . 
    ( Env.Read m, Defs.Read m, ContainsTerms t  
    , Substitutable a, Inner a ~ Term ) =>
  Set Term -> t -> (Indices.Shift -> t -> m a) -> m a
generaliseTerms (toList -> terms) target run
  | length terms == 0 = run id target
  | otherwise = do
    gen_bs <- mapM makeBind (range terms)
          
    -- Run the inner computation
    done_t <- id
      . Env.bindMany (reverse gen_bs)
      . run liftHere 
      . mapTerms (generalise gen_bs)
      $ target
      
    -- Reverse the generalisation
    return 
      . foldr Indices.subst done_t
      $ zipWith Indices.liftMany [0..] (reverse terms)
  where
  generalise :: [Bind] -> Term -> Term
  generalise gen_bs = id
    . concatEndos (zipWith replace terms' new_vars)
    . liftHere
    where
    terms' = map liftHere terms
    new_vars = zipWith (Var . enum) [0..length terms - 1] gen_bs
    
  makeBind :: Nat -> m Bind
  makeBind n
    | Var x _ <- terms !! n = Env.boundAt x
  makeBind n = do
    ty <- Type.getM (terms !! n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (nlength terms)
  
  
-- | Finds uninterpreted function calls and generalises them.
generaliseUninterpreted :: ( Env.Read m, Defs.Read m, ContainsTerms t
                           , Substitutable a, Inner a ~ Term ) =>
  t -> (Indices.Shift -> t -> m a) -> m a
generaliseUninterpreted target =
  generaliseTerms f_calls target
  where
  f_calls = concatMap (collect functionCall) (containedTerms target)
    where
    functionCall (App (Var f _) _) = True
    functionCall _ = False
    
  
-- | Construct an n-tuple of the given terms. Needs to read the type of the
-- terms so it can construct the appropriate n-tuple type for
-- the constructor.
tuple :: (Defs.Read m, Env.Read m) => [Term]-> m Term
tuple ts
  | length ts > 1 = do
    ind <- id
      . liftM Type.tuple
      $ mapM Type.getM ts
    return (app (Con (Tag.with Tag.null (Type.Constructor ind 0))) ts)
 
    
-- | Take a free variable of a fixpoint and express it as a new first argument
-- of that fixpoint.
-- It can reverse the @constArg@ step from "Elea.Transform.Simplify".
-- Necessary for then @freeDecreasingArg@ step from "Elea.Transform.Fusion".
expressFreeVariable :: Env.Read m => Index -> Term -> m Term
expressFreeVariable free_var (Fix fix_i (Bind fix_n fix_ty) fix_t) = do
  var_b <- Env.boundAt free_var
  let fix_ty' = Type.Fun (get Type.bindType var_b) fix_ty
      fix_b = Bind fix_n fix_ty'
  return
    . (\t -> app t [Var free_var var_b])
    . Fix fix_i fix_b
    . Lam var_b
    . Env.trackIndices (Var 0 var_b, Var 1 fix_b)
    . Fold.transformM update
    $ Indices.lift fix_t
  where
  update :: Term -> Env.TrackIndices (Term, Term) Term
  -- Update function calls
  update term@(App (Var f _) args) = do
    (new_var, new_fix) <- Env.tracked
    if f == varIndex new_fix
    then return (app new_fix (new_var : args))
    else return term
  -- Update variables occurrences
  update (Var x b) = do
    (Var x' b', _) <- Env.tracked
    if x == free_var + x' + 2
    then return (Var x' b')
    else return (Var x b)
  update other = 
    return other

-- | The fold of 'expressFreeVariable' over a list of indices.
-- The order of the new variables applied to the output term will match
-- the order of the indices input.
-- > expressFreeVariables [x, y, z] (fix F) = (fix G) x y z
expressFreeVariables :: forall m . Env.Read m => [Index] -> Term -> m Term
expressFreeVariables = flip (foldrM express) 
  where
  express :: Index -> Term -> m Term
  express free_var (flattenApp -> fix : args) = do
    App fix' [new_arg] <- expressFreeVariable free_var fix
    return (app fix' (new_arg : args))
    
    
-- | A /finite match/ in one in which any recursively typed variables
-- bound down any pattern branch are unused. So if a finite match over a list
-- does not reference the sub-list.
-- Could be extended to depth greater than one, viz. matching over a list
-- will only analyse a finite portion of the list.
isFiniteMatch :: [Alt] -> Bool
isFiniteMatch = all recArgsUsed
  where
  recArgsUsed :: Alt -> Bool
  recArgsUsed (Alt tcon _ alt_t) = 
    Set.null (Set.intersection (Indices.free alt_t) rec_args)
    where
    rec_args = id
      . Set.fromList 
      . Type.recursiveArgIndices 
      $ Tag.untag tcon
    

commuteMatchesWhenM :: forall m . Env.Write m 
  => (Term -> Term -> m Bool) -> Term -> m Term
commuteMatchesWhenM when = Fold.rewriteM commute
  where
  commute :: Term -> MaybeT m Term
  commute outer_cse@(Case outer_t outer_as) = do
    mby_inner_cse <- id
      . lift
      . firstM 
      $ map (runMaybeT . commutable) outer_as
    Fail.unless (isJust mby_inner_cse)
    let Just inner_cse = mby_inner_cse
    return (applyCase inner_cse outer_cse)
    where
    commutable :: Alt -> MaybeT m Term
    commutable (Alt con bs (Case inner_t inner_as))
      | Indices.lowerableBy (nlength bs) inner_t = do
        here <- lift (when outer_t inner_t')
        Fail.unless here
        return (Case inner_t' inner_as)
      where
      inner_t' = Indices.lowerMany (nlength bs) inner_t
    commutable _ = 
      Fail.here
  commute _ = 
    Fail.here
    
unifyArgs :: [Nat] -> Term -> Term
unifyArgs [] term = term
unifyArgs [n] term = term
unifyArgs (n:ns) term =
  unflattenLam arg_bs body_t'
  where
  (arg_bs, body_t) = flattenLam term
  arg_idxs :: [Index] = id
    . reverse
    . map enum
    $ range arg_bs
  main_var = Var (arg_idxs !! n) (arg_bs !! n)
  
  unify m = Indices.replaceAt (arg_idxs !! m) main_var
  body_t' = foldr unify body_t ns


-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> Env.trackeds (== t))

-- | Whether the first argument is a subterm of the second
isSubterm :: Term -> Term -> Bool
isSubterm t = Env.trackIndices t . Fold.anyM (\t -> Env.trackeds (== t))
    
isStrictSubterm :: Term -> Term -> Bool
isStrictSubterm t t' = t `isSubterm` t' && t /= t'

{-
-- | Finds a context which will turn the first term into the second.
-- Basically takes the first term and replaces all instances of it 
-- with the gap in the second. Will fail if there was not at least one
-- instance.
findContext :: Fail.Can m => Term -> Term -> m Context
findContext inner full = do
  Fail.unless (inner `isSubterm` full)
  return (Context.make (\gap -> replace inner gap full))
  -}
  
-- | Removes all elements which are subterms of other elements in the list
removeSubterms :: [Term] -> [Term]
removeSubterms = foldr remove []
  where
  remove t ts
    | any (isSubterm t) ts = ts
    | otherwise = t : filter (not . (`isSubterm` t)) ts
  

-- | Returns all the subterms of a term which contain free variables and nothing
-- but free variables.
freeSubtermsOf :: Term -> Set Term
freeSubtermsOf term = id
  . Set.fromList
  -- Remove any terms which are subterms of another term in the set
  . removeSubterms
  -- Make sure they actually contain free variables
  . filter (not . Set.null . Indices.free)
  . Set.toList
  -- We only want strict subterms
  . Set.delete term
  -- Collect all free subterms
  . collect (const True)
  $ term
    

-- | All free variables in a term
freeVars :: Term -> Set Term
freeVars = collect isVar
    

-- | If we pattern match on the result of recursive call to a fixpoint
-- we should float that as far inside the term as possible.
floatRecCallInwards :: Term -> Term
floatRecCallInwards = 
  Fold.transform inwards
  where
  inwards (Fix i b fix_t) =
    Fix i b fix_t'
    where 
    fix_t' = id
      . Env.trackIndices 0
      $ commuteMatchesWhenM isRecCall fix_t
      
    isRecCall :: Term -> Term -> Env.TrackIndices Index Bool
    isRecCall outer_t (leftmost -> Var f _) = do
      fix_f <- Env.tracked
      return (f == fix_f && not (isVar (leftmost outer_t)))
    isRecCall _ _ = return False
      
  inwards term = term
    

instance Tag.Has Term where
  tags = Fold.collect tags'
    where
    tags' :: Term -> Maybe Tag 
    tags' (Fix inf _ _) = Just (get fixIndex inf)
    tags' _ = Nothing
    
  map f = Fold.transform rep 
    where
    rep :: Term -> Term
    rep (Fix inf b t) = 
      Fix (modify fixIndex f inf) b t
    rep t = t
    

-- | Check that all argument lambdas are topmost in a fixed-point
isLambdaFloated :: Term -> Bool
isLambdaFloated fix@(Fix _ _ fix_t) = 
  ty_arg_count == lam_count
  where
  ty_arg_count = length (Type.argumentTypes (Type.get fix))
  lam_count = length (fst (flattenLam fix_t))
      
  
-- | Attempt to find a list of arguments for the first term that
-- will make it equal the second
findArguments :: Fail.Can m => Term -> Term -> m [Term]
findArguments ctx term = do
  uni <- Unifier.find ctx_body (Indices.liftMany (nlength arg_bs) term)
  Fail.unless (Unifier.domain uni `Set.isSubsetOf` Set.fromList arg_idxs)
  return 
    . map (Indices.lowerMany (nlength arg_bs) . snd)
    $ Map.toDescList (Map.union uni defaults)
  where
  (arg_bs, ctx_body) = flattenLam ctx
  
  arg_idxs :: [Index]
  arg_idxs = (map enum . range) arg_bs
  
  defaults = id
    . Map.fromList 
    . zipWith (\idx (Bind _ ty) -> (idx, Bot ty)) arg_idxs 
    $ reverse arg_bs
  
  
findConstrainedArgs :: forall m . (Env.MatchRead m, Fail.Can m) 
  => Term -> Term -> m [Term]
findConstrainedArgs ctx term
  | not (Constraint.has ctx) = findArguments ctx term
  -- ^ If this holds the term context is not actually constrained
  -- and so we can use regular argument finding
  
  | otherwise = do
    cs <- Env.findConstraints usefulConstraint
    Fail.choose (map tryConstraint cs)
    
  where
  usefulConstraint :: Constraint -> Bool
  usefulConstraint ct = 
    Constraint.to ctx == get patternConstructor (matchedPattern ct)
    
  tryConstraint :: Fail.Can m => Constraint -> m [Term]
  tryConstraint ct = id
    . findArguments ctx 
   -- . traceMe "trying"
    $ Constraint.apply (Type.get term) ct term

  
-- | Beta-abstracts the given index
abstractVar :: Bind -> Index -> Term -> Term
abstractVar b x t = id
  . Lam b 
  . Indices.replaceAt (succ x) (Var 0 b) 
  $ Indices.lift t

abstractVars :: [Bind] -> [Index] -> Term -> Term
abstractVars bs xs = 
  concatEndos (zipWith abstractVar bs xs)
  
abstractTerm :: Term -> Term -> Term
abstractTerm abs_t in_t = id
  . Lam abs_b 
  . replace (Indices.lift abs_t) (Var 0 abs_b) 
  $ Indices.lift in_t
  where
  abs_b = Bind "g" (Type.get abs_t)

mapFixInfo :: (FixInfo -> FixInfo) -> Term -> Term
mapFixInfo f = Fold.transform mp
  where
  mp (Fix i b t) = Fix (f i) b t
  mp t = t
  

-- > equateArgs 0 2 (\a b c d -> C[a][b][c][d]) = (\a b d -> C[a][b][a][d])
equateArgs :: Nat -> Nat -> Term -> Term 
equateArgs i j orig_t = id
  . assert (i < j)
  . assert (j < nlength bs)
  $ unflattenLam new_bs new_body
  where
  (bs, body_t) = flattenLam orig_t
  new_bs = removeAt j bs
  new_body = Indices.substAt (toIdx j) (Var (pred (toIdx i)) (bs !! i)) body_t
  
  toIdx :: Nat -> Index
  toIdx x = id
    . assert (x < nlength bs) 
    $ enum ((nlength bs - x) - 1)
  
  
equateArgsMany :: [(Nat, Nat)] -> Term -> Term
equateArgsMany ijs t =
  foldr (uncurry equateArgs) t (sortBy (compare `on` snd) ijs)
  
  
revertMatches :: Env.MatchRead m => Term -> m Term
revertMatches term = do
  ms <- Env.matches
  let unambig_ms = id
        . filter (\(from, _) -> (not . null . arguments) from)
        -- ^ Without this we could rewrite things like "True", which would
        -- be ambiguous
        . map (\m -> (matchedTo m, matchedTerm m))
        $ ms
  return (replaceAll unambig_ms term)

 
-- | Assumes the second argument is a =< and tries to generalise the first
-- argument on either side of the relation, but only if it exists on both sides.
-- Otherwise this just returns the second argument.
tryGeneralise :: Term -> Term -> Term
tryGeneralise gen_t (Leq x y)
  | gen_t `isSubterm` x 
  , gen_t `isSubterm` y
  , Type.has gen_t =
    Lam gen_b (Leq (gen x) (gen y))
  where
  gen_b = Bind "g" (Type.get gen_t)
  gen z = replace (Indices.lift gen_t) (Var 0 gen_b) (Indices.lift z)
tryGeneralise _ t = t
  

-- | Like tryGeneralise but takes a variable and only generalises instances
-- which are free in fixed-points. Used for the generalisation required
-- by the discovery step within some free-variable fusion steps.
tryGeneraliseInFix :: Env.Read m => Index -> Term -> m Term
tryGeneraliseInFix var_t leq@(Leq {}) = do
  gen_b <- Env.boundAt var_t
  let leq' = id
        . Env.trackIndices (Indices.lift var_t, Var 0 gen_b) 
        . Fold.transformM genInFix
        $ Indices.lift leq
  return (Lam gen_b leq')
  where
  genInFix :: Term -> Env.TrackIndices (Index, Term) Term
  genInFix fix@(Fix {}) = do
    (from_x, to_x) <- Env.tracked
    return (Indices.replaceAt from_x to_x fix)
  genInFix other = 
    return other


-- | Whether the first term is pattern matched upon in the second
matchedWithin :: Term -> Term -> Bool
matchedWithin t = id
  . Env.trackIndices t
  . Fold.anyM matched
  where
  matched :: Term -> Env.TrackIndices Term Bool
  matched (Case cse_t _) = do
    t <- Env.tracked
    return (t == cse_t)
  matched _ = return False


buildContext :: forall m . Env.Read m => Int -> Term -> m (Term, [Term])
buildContext arg_i (App fix@(Fix _ fix_b fix_t) args) = do
  arg_bs <- zipWithM getArgBind [0..] arg_xs
  return ( build free_bs arg_bs, args' )
  where
  free_vars = Set.toList (freeVars fix Set.\\ Set.unions (map freeVars args))
  free_bs = map binding free_vars
  arg_f : arg_xs = flattenApp (args !! arg_i)
  args' = free_vars ++ removeAt (enum arg_i) args ++ arg_xs
  
  getArgBind :: Int -> Term -> m Bind
  getArgBind _ (Var x _) = 
    -- TODO can replace with new bind + assertion
    Env.boundAt x
  getArgBind n t = do
    ty <- Type.getM t
    return (Bind ("x" ++ show n) ty)
  
  unify_me = id
    . map (map snd)
    . filter ((>= 2) . length)
    . groupBy ((==) `on` fst)
    . sort
    $ zip args' [0..]
    
  build free_bs arg_bs = id 
    . flip (foldr unifyArgs) unify_me 
    . unflattenLam full_bs 
    $ App fix' args'
    where
    fix_bs = removeAt (enum arg_i) (fst (flattenLam fix_t))
    full_bs = free_bs ++ fix_bs ++ arg_bs

    fix' = id
      . Indices.liftMany (enum full_c)
      . snd
      . flattenLam
      $ abstractVars free_bs (map fromVar free_vars) fix
      
    mkVar :: Enum a => a -> Term
    mkVar x = Var (enum x) (reverse full_bs !! x)

    args' = left_args ++ [arg'] ++ right_args
      
    left_args =
      reverse $ map (mkVar . (+ arg_c)) [0..arg_i-1]
    right_args = 
      reverse $ map (mkVar . (+ arg_c)) [arg_i..fix_c-1]
    
    arg' = App arg_f' arg_xs'
    arg_xs' = reverse (map mkVar [0..arg_c-1])
    arg_f' = Indices.liftMany (enum (full_c + length free_bs)) arg_f

    arg_c = length arg_bs
    fix_c = length fix_bs
    full_c = fix_c + arg_c
    

buildCase :: Env.Read m => Term -> Term -> m Term
buildCase match_t branch_t = do
  Type.Base match_ind <- Type.getM match_t
  let cons = Type.constructors match_ind
  return (Case match_t (map buildAlt cons))
  where
  buildAlt :: Constructor -> Alt
  buildAlt con = 
    Alt tcon bs (Indices.liftMany (nlength bs) branch_t)
    where
    tcon = Tag.Tagged Tag.null con               
    bs = Type.makeAltBindings con
    
isProductive :: Term -> Bool
isProductive (Fix _ _ fix_t) = id
  . Env.trackOffset
  $ Fold.isoAllM branches prod fix_t
  where
  prod :: Term -> Env.TrackOffset Bool
  prod (leftmost -> t) =
    if isCon t 
    then return True
    else return False
    {-do
      fix_f :: Index <- liftM enum Env.tracked
      return (not (fix_f `Indices.freeWithin` t))-}
      