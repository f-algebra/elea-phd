-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Term.Ext
(
  module Elea.Term,
  branches,
  recursionScheme,
  replace,
  unfoldFix,
  unwrapFix,
  collectM, collect,
  decreasingArgs,
  decreasingAppArgs,
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
  commuteMatchesWhenM,
  occurrences,
  isSubterm,
  removeSubterms,
  freeSubtermsOf,
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
  strictArgs
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
strictWithin (Eql x y) = 
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
    . Set.mapMonotonic (Indices.lowerMany (length bs))
    . Set.filter (Indices.lowerableBy (length bs))
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
  toArgPos idx = (length arg_bs - enum idx) - 1
  
  strict_vars :: [Index]
  strict_vars = id
    . map fromVar
    . filter isVar
    . Set.toList
    $ strictWithin fix_body
    

-- | Replace all instances of one term with another within a term.
replace :: Term -> Term -> Term -> Term
replace me with = id
  . Env.trackIndices (me, with)
  . Fold.transformM doReplace
  where
  -- 'Env.TrackIndices' is needed to make sure indices
  -- are properly updated as we move inside 
  -- the term, e.g. if we pass inside a lambda.
  doReplace :: Term -> Env.TrackIndices (Term, Term) Term
  doReplace term = do
    (me, with) <- Env.tracked
    if term == me
    then return with
    else return term
  

-- | A wrapped around 'decreasingArgs' which takes a fixpoint with arguments
-- applied and removes any return indices which are greater than the length
-- of the arguments.
-- Helps stop 'decreasingArgs' causing errors on partially applied 
-- fixpoints.
decreasingAppArgs :: Term -> [Int]
decreasingAppArgs (App fix args) = 
  filter (length args >) (decreasingArgs fix)
  

-- | Returns the indices of the strictly decreasing arguments for
-- a given function. Undefined if not given a 'Fix'.
decreasingArgs :: Term -> [Int]
decreasingArgs (Fix _ fix_b fix_t) = 
  filter isDecreasing [0..length arg_bs - 1]
  where
  (arg_bs, fix_body) = flattenLam fix_t
  
  isDecreasing :: Int -> Bool
  isDecreasing arg_i = id
    . Env.trackIndices fix_f
    
    -- We track all terms which are 
    -- structurally smaller than our starting argument
    . Env.trackSmallerThan (Var arg_idx)
    $ Fold.allM decreasing fix_body
    where
    -- The deBrujin index of the lambda bound variable we are tracking
    arg_idx = enum (length arg_bs - (arg_i + 1))
    
    -- The deBrujin index of the fix bound function variable
    fix_f = length arg_bs
    
    decreasing :: 
      Term -> Env.TrackSmallerTermsT (Env.TrackIndices Index) Bool
    decreasing t@(App (Var f) args) = do
      fix_f <- Trans.lift Env.tracked
      if fix_f /= f || arg_i >= length args
      then return True
      else Env.isSmaller (args `nth` arg_i)
    decreasing _ = 
      return True
      
  
-- | Take a case-of term and replace the result term down each branch
-- with the second term argument.
applyCase :: Term -> Term -> Term
applyCase (Case cse_t alts) inner_t = 
  Case cse_t (map mkAlt alts)
  where
  mkAlt :: Alt -> Alt
  mkAlt (Alt con bs _) = 
    Alt con bs (Indices.liftMany (length bs) inner_t)
    
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
  gen_bs <- mapM makeBind [0..length args - 1] 
        
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
  new_vars = map Var [0..length args - 1]
  
  makeBind :: Int -> m Bind
  makeBind n
    | Var x <- args `nth` n = Env.boundAt x
  makeBind n = do
    ty <- Type.getM (args `nth` n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (length args)
  
  
-- | Like 'generaliseArgs' but generalises /every/ occurrence of a set of terms
-- within another.
generaliseTerms :: forall m a t . 
    ( Env.Read m, Defs.Read m, ContainsTerms t  
    , Substitutable a, Inner a ~ Term ) =>
  Set Term -> t -> (Indices.Shift -> t -> m a) -> m a
generaliseTerms (toList -> terms) target run
  | length terms == 0 = run id target
  | otherwise = do
    gen_bs <- mapM makeBind [0..length terms - 1]
          
    -- Run the inner computation
    done_t <- id
      . Env.bindMany (reverse gen_bs)
      . run liftHere 
      . mapTerms generalise
      $ target
      
    -- Reverse the generalisation
    return 
      . foldr Indices.subst done_t
      $ zipWith Indices.liftMany [0..] (reverse terms)
  where
  generalise :: Term -> Term
  generalise = id
    . concatEndos (zipWith replace terms' new_vars)
    . liftHere
    where
    terms' = map liftHere terms
    new_vars = map (Var . enum) [0..length terms - 1]
    
  makeBind :: Nat -> m Bind
  makeBind n
    | Var x <- terms !! n = Env.boundAt x
  makeBind n = do
    ty <- Type.getM (terms `nth` n)
    let name = "_" ++ show ty
    return (Bind name ty)
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (length terms)
  
  
-- | Finds uninterpreted function calls and generalises them.
generaliseUninterpreted :: ( Env.Read m, Defs.Read m, ContainsTerms t
                           , Substitutable a, Inner a ~ Term ) =>
  t -> (Indices.Shift -> t -> m a) -> m a
generaliseUninterpreted target =
  generaliseTerms f_calls target
  where
  f_calls = concatMap (collect functionCall) (containedTerms target)
    where
    functionCall (App (Var f) _) = True
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
  return
    . (\t -> app t [Var free_var])
    . Fix fix_i (Bind fix_n fix_ty')
    . Lam var_b
    . Env.trackOffset
    . Fold.transformM update
    $ Indices.lift fix_t
  where
  update :: Term -> Env.TrackOffset Term
  -- Update function calls
  update term@(App (Var f) args) = do
    idx <- Env.tracked
    if f == succ idx
    then return (app (Var (succ idx)) (Var idx : args))
    else return term
  -- Update variables occurrences
  update (Var x) = do
    idx <- Env.tracked
    if x == free_var + idx + 2
    then return (Var idx)
    else return (Var x)
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
      $ Tag.tagged tcon
    

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
      | Indices.lowerableBy (length bs) inner_t = do
        here <- lift (when outer_t inner_t')
        Fail.unless here
        return (Case inner_t' inner_as)
      where
      inner_t' = Indices.lowerMany (length bs) inner_t
    commutable _ = 
      Fail.here
  commute _ = 
    Fail.here


-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> Env.trackeds (== t))

-- | Whether the first argument is a subterm of the second
isSubterm :: Term -> Term -> Bool
isSubterm t = Env.trackIndices t . Fold.anyM (\t -> Env.trackeds (== t))
    
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
    isRecCall outer_t (leftmost -> Var f) = do
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
  uni <- Unifier.find ctx_body (Indices.liftMany (length arg_bs) term)
  Fail.unless (Indices.free uni `Set.isSubsetOf` arg_idxs)
  return 
    . map (Indices.lowerMany (length arg_bs) . snd)
    $ Map.toDescList uni
  where
  (arg_bs, ctx_body) = flattenLam ctx
  
  arg_idxs :: Set Index
  arg_idxs = (Set.fromList . map enum) [0..length arg_bs - 1]
  
  
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
  . Indices.replaceAt (succ x) (Var 0) 
  $ Indices.lift t

abstractVars :: [Bind] -> [Index] -> Term -> Term
abstractVars bs xs = 
  concatEndos (zipWith abstractVar bs xs)
  

mapFixInfo :: (FixInfo -> FixInfo) -> Term -> Term
mapFixInfo f = Fold.transform mp
  where
  mp (Fix i b t) = Fix (f i) b t
  mp t = t
  

-- > equateArgs 0 2 (\a b c d -> C[a][b][c][d]) = (\a b d -> C[a][b][a][d])
equateArgs :: Nat -> Nat -> Term -> Term 
equateArgs i j orig_t
  | assert (i < j) True
  , assert (j < length bs) True =
    unflattenLam new_bs new_body
  where
  (bs, body_t) = flattenLam orig_t
  new_bs = removeAt j bs
  new_body = Indices.substAt (toIdx j) (Var (pred (toIdx i))) body_t
  
  toIdx :: Nat -> Index
  toIdx x = enum ((elength bs - x) - 1)
  
  
equateArgsMany :: [(Nat, Nat)] -> Term -> Term
equateArgsMany ijs t =
  foldr (uncurry equateArgs) t (sortBy (compare `on` snd) ijs)
  
