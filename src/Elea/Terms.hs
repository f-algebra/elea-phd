-- | Here I've put all the helpful functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  isBaseCase, isFinite, isRecursiveInd,
  recursiveInjArgs, recursivePatternArgs,
  replace, contains, nthArgument,
  collectM, occurrences,
  generalise, generaliseMany, buildCaseOf,
  revertMatchesWhen, descendWhileM,
  isProductive,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Index
import Elea.Term
import Elea.Show
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Typing as Typing
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error  as Err
import qualified Data.Set as Set
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

-- | For a given inductive type, return whether the constructor at that 
-- index is a base case.
isBaseCase :: Type -> Nat -> Bool
isBaseCase (Ind _ cons) = id
  . not
  . Set.member 0
  . Indices.free
  -- These next three strip the return type out, since it will always contain
  -- an instance of the defined inductive type.
  . uncurry unflattenPi
  . second (const Absurd)
  . flattenPi
  . get boundType
  . (cons !!)
  . fromEnum
  
-- | For a given inductive type and constructor number this returns the
-- argument positions which are recursive.
recursiveInjArgs :: Type -> Nat -> Set Int
recursiveInjArgs (Ind _ cons) inj_n = id
  . mconcat
  . zipWith isRec [0..]
  . map (get boundType)
  $ args
  where
  con = cons !! fromEnum inj_n
  (args, _) = flattenPi (get boundType con)
  
  isRec :: Int -> Type -> Set Int
  isRec x ty 
    | toEnum x `Set.member` Indices.free ty = Set.singleton x
    | otherwise = mempty
    
-- | This is just the arguments of 'altPattern' at
-- the positions from 'recursiveInjArgs' 
recursivePatternArgs :: Type -> Nat -> Set Index
recursivePatternArgs ty n = 
  Set.map (fromVar . (args !!)) (recursiveInjArgs ty n)
  where
  args = tail (flattenApp (altPattern ty n))
  
isRecursiveInd :: Type -> Bool
isRecursiveInd ty@(Ind _ cons) = 
  not . all (isBaseCase ty) . map toEnum $ [0..length cons - 1]

-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (flattenApp -> Inj n ind_ty : args) = 
  all isFinite rec_args
  where
  rec_args = Set.map (args !!) (recursiveInjArgs ind_ty n)
isFinite _ = False

nthArgument :: Int -> Type -> Type
nthArgument n = id
  . Indices.lowerMany n
  . get boundType
  . (!! n)
  . fst
  . flattenPi 

-- | Replace all instances of one term with another.
replace :: ContainsTerms t => Term -> Term -> t -> t
replace me with = id
  . Env.trackIndices (me, with)
  . mapTermsM (Fold.transformM apply)
  where
  apply :: Term -> Env.TrackIndices (Term, Term) Term
  apply term = do
    (me, with) <- ask
    if term == me
    then return with
    else return term

-- | If the first argument is a subterm of the second
contains :: Term -> Term -> Bool
contains sub = Env.trackIndices sub . Fold.anyM (\t -> asks (== t))
    
-- | Takes a term to generalise, and modifies a term to term function,
-- such that the supplied term is generalised going in,
-- and ungeneralised coming out. Just treat t1 and t2 as Term, 
-- I had to generalise it to t1 and t2 for annoying reasons.
generalise :: (Env.Readable m, ContainsTerms t1, ContainsTerms t2,
    KleisliShow t1, ShowM t1 m) => 
  Term -> (Index -> t1 -> m t2) -> (t1 -> m t2)
generalise gen_t transform term = do
  -- First we create a binding for the new variable by finding its type
  -- and creating a descriptive label.
  gen_ty <- Err.noneM (Typing.typeOf gen_t)
  lbl <- liftM (\t -> "{" ++ t ++ "}") (showM gen_t)
  let gen_b = Bind (Just lbl) gen_ty
  
  -- Generalise by replacing all instances of the term with a new variable
  -- at index 0 and make room for this index by lifting the original term.
  let term' = id 
        . replace (Indices.lift gen_t) (Var 0)
        $ Indices.lift term
  
  -- Apply our term to term function, with the new variable bound.
  term'' <- Env.bindAt 0 gen_b (transform 0 term') 
  
  -- Finally, we reverse the generalisation process.
  return 
    . Indices.lower
    . Indices.replaceAt 0 (Indices.lift gen_t)
    $ term''
    
generaliseMany :: forall m t1 t2 . 
  (Env.Readable m, ContainsTerms t1, ContainsTerms t2,
    KleisliShow t1, ShowM t1 m) => 
  [Term] -> ([Index] -> t1 -> m t2) -> (t1 -> m t2)
generaliseMany ts f = foldr gen f lifted_ts []
  where
  lifted_ts = zipWith Indices.liftMany [0..] ts
  
  gen :: Term -> ([Index] -> t1 -> m t2) -> ([Index] -> t1 -> m t2)
  gen t f ixs = 
    generalise t (\ix -> f (ix : map Indices.lift ixs))
    
-- | Pattern matches are automatically applied as replacements down
-- every branch. This will revert any such replacements of any term
-- that fulfils the given predicate.
revertMatchesWhen :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m  Term
revertMatchesWhen when = Fold.transformM revert
  where 
  revert :: Term -> m Term
  revert term@(Case cse_t ind_ty alts) = do
    here <- when cse_t
    if not here
    then return term
    else do
      let alts' = zipWith revertAlt [0..] alts
      return (Case cse_t ind_ty alts')
    where
    revertAlt :: Nat -> Alt -> Alt
    revertAlt n (Alt bs alt_t) = Alt bs alt_t'
      where
      rep_t = Indices.liftMany (length bs) cse_t
      alt_t' = replace (altPattern ind_ty n) rep_t alt_t
  revert other = 
    return other
  
-- | Some simple code shared by descendWhileM and mapBranchesM.
descendAlt :: Alt -> Alt' (Bool, Term)
descendAlt (Alt bs alt_t) = Alt' bs' (True, alt_t)
  where
  bs' = map (fmap (\t -> (False, t)) . projectBind) bs
  
-- | Decends into non-type subterms of a term while a predicate holds.
descendWhileM :: forall m . Fold.FoldM Term m => 
  (Term -> m Bool) -> (Term -> m Term) -> Term -> m Term
descendWhileM when = Fold.descendM while
  where
  while :: Term -> m (Bool, Term' (Bool, Term))
  while term = do
    recurse <- when term
    if not recurse
    then return (True, fmap (\t -> (False, t)) (Fold.project term))
    else return (descend term)
    where
    descend :: Term -> (Bool, Term' (Bool, Term))
    descend (Case cse_t ind_ty alts) = 
      (False, Case' (True, cse_t) (False, ind_ty) (map descendAlt alts))
      where
      
    descend (Lam (Bind lbl ty) t) = 
      (False, Lam' (Bind' lbl (False, ty)) (True, t))
      
    -- TODO: Missing cases here

-- | Applies a given transformation function to the innermost branches
-- of a term made of pattern matches.
-- Will also move inside lambda abstractions.
mapBranchesM :: forall m . Env.Writable m => 
  (Term -> m Term) -> Term -> m Term
mapBranchesM = Fold.descendM (return . branches)
  where
  branches :: Term -> (Bool, Term' (Bool, Term))
  branches (Case cse_t ind_ty alts) =
    (False, Case' (False, cse_t) (False, ind_ty) (map descendAlt alts))
  branches (Lam (Bind lbl ty) t) = 
    (False, Lam' (Bind' lbl (False, ty)) (True, t))
  branches term = 
    (True, fmap (\t -> (False, t)) (Fold.project term))

foldBranchesM :: (Env.Writable m, Monoid w) => (Term -> m w) -> Term -> m w
foldBranchesM f = execWriterT . mapBranchesM fold
  where
  fold t = do
    t |> f |> Trans.lift >>= tell
    return t

allBranchesM :: Env.Writable m => (Term -> m Bool) -> Term -> m Bool
allBranchesM p = liftM Monoid.getAll . foldBranchesM (liftM Monoid.All . p)

allBranches :: (Term -> Bool) -> Term -> Bool
allBranches p = runIdentity . allBranchesM (Identity . p)

collectM :: forall m . Env.Writable m => 
  (Term -> m Bool) -> Term -> m (Set Term)
collectM p = Env.alsoTrack 0 . Fold.collectM collect
  where
  collect :: Term -> MaybeT (Env.AlsoTrack Index m) Term
  collect t = do
    c <- Trans.lift . Trans.lift $ p t
    guard c
    offset <- Trans.lift ask
    return (Indices.lowerMany (fromEnum offset) t)
    
-- | Return the number of times a given subterm occurs in a larger term.
occurrences :: Term -> Term -> Int
occurrences t = Env.trackIndices t . Fold.countM (\t -> asks (== t))
  
buildCaseOf :: Term -> Type -> (Nat -> Term) -> Term
buildCaseOf cse_of ind_ty@(Typing.unfoldInd -> cons) mkAlt = 
  Case cse_of ind_ty (zipWith buildAlt [0..] cons)
  where
  buildAlt :: Nat -> Bind -> Alt
  buildAlt n bind = Alt bs alt_t
    where
    bs = fst . flattenPi . get boundType $ bind
    alt_t = Indices.liftMany (length bs) (mkAlt n)

-- | A function is "productive" if it will return something in HNF
-- if unrolled once. We implement this by checking for a constructor
-- topmost down every pattern match branch.
isProductive :: Term -> Bool
isProductive (Fix _ _ fix_t) = 
  allBranches (isInj . leftmost) fix_t
