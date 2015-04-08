module Elea.Term
(
  module Elea.Index,
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Type, Bind (..), Constructor (..),
  Constraint (..), FixInfo (..),
  ContainsTerms (..), mapTerms, containedTerms,
  Equation (..), equationName, equationLHS, equationRHS,
  app,
  projectAlt, embedAlt,
  altBindings, altInner, altConstructor,
  altBindings', altInner', altConstructor',
  fixClosed, fixName, fixTag,
  constrainedTerm, constrainedTo,
  flattenApp, leftmost, arguments,
  flattenLam, unflattenLam,
  isCon, isLam, isVar,
  isFix, isUnr, isCase,
  isUnr',
  emptyInfo,
  tags,
  inductivelyTyped, 
  fromVar, 
  conjunction, true, false,
  altPattern, recursivePatternVars,
  isSimple,
  isFinite,
  lowerableAltInner,
  loweredAltInner,
  buildFold,
  buildEq,
)
where

import Elea.Prelude
import Elea.Index ( Index, Indexed, Substitutable, Inner )
import Elea.Type ( Type (..), Ind (..), ConArg (..)
                 , Bind (..), Constructor (..) )
import Elea.Tag ( Tag )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Tag as Tag
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Algebra.Lattice as Algebra


data Term
  = Unr     { resultType :: !Type } 
  
  | Var     { varIndex :: !Index }

  | App     !Term ![Term]

  | Lam     { binding :: !Bind 
            , inner :: !Term }

  | Fix     { fixInfo :: !FixInfo
            , binding :: !Bind 
            , inner :: !Term }

  | Con     { constructor :: !Constructor }

  | Case    { caseOf :: !Term
            , caseAlts :: ![Alt] }
  deriving ( Eq, Ord )

  
data Alt
  = Alt     { _altConstructor :: !Constructor
            , _altBindings :: ![Bind]
            , _altInner :: !Term }
  deriving ( Eq, Ord )
  
  
-- | A constraint is a pattern match where only one branch is non-absurd.
-- Functions surrounding constraints can be found in "Elea.Constraint".
data Constraint 
  = Constraint  { _constrainedTo :: !Constructor 
                , _constrainedTerm :: !Term }
  deriving ( Eq, Ord )
  
  
-- | Information stored about fixpoints, to add efficiency.
data FixInfo
  = FixInfo { _fixClosed :: !Bool 
            , _fixName :: Maybe String
            , _fixTag :: Tag }
            
              
-- Fixpoint information does not change the meaning of a fixpoint, so
-- it does not effect equality between terms.
instance Eq FixInfo where
  _ == _ = True
instance Ord FixInfo where
  _ `compare` _ = EQ
  
-- | Equations between terms
data Equation
  = Equals  { _equationName :: String
            , _equationVars :: [Bind]
            , _equationLHS :: Term
            , _equationRHS :: Term }

 
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Unr' !Type
  | Var' !Index
  | App' a [a]
  | Lam' !Bind a
  | Fix' !FixInfo !Bind a
  | Con' !Constructor
  | Case' a ![Alt' a]
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altConstructor' :: !Constructor
         , _altBindings' :: ![Bind]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Alt, ''Alt', ''FixInfo, ''Equation, ''Constraint ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt c bs t) = Alt' c bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' c bs t) = Alt c bs t

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App f xs) = App' f xs
  project (Lam b t) = Lam' b t
  project (Fix i b t) = Fix' i b t
  project (Con con) = Con' con
  project (Case t alts) = Case' t (map projectAlt alts)
  project (Unr ty) = Unr' ty

instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' f xs) = App f xs
  embed (Lam' b t) = Lam b t
  embed (Fix' i b t) = Fix i b t
  embed (Con' con) = Con con
  embed (Case' t alts) = Case t (map embedAlt alts)
  embed (Unr' ty) = Unr ty
  
class ContainsTerms t where
  mapTermsM :: Monad m => (Term -> m Term) -> t -> m t
  
mapTerms :: ContainsTerms t => (Term -> Term) -> t -> t
mapTerms f = runIdentity . mapTermsM (return . f)

containedTerms :: ContainsTerms t => t -> [Term]
containedTerms = execWriter . mapTermsM tellTerm
  where
  tellTerm t = tell [t] >> return t
  
instance ContainsTerms Term where
  mapTermsM = ($)
  
instance ContainsTerms Equation where
  mapTermsM f = modifyM equationLHS f <=< modifyM equationRHS f
  
instance (ContainsTerms a, ContainsTerms b) => ContainsTerms (a, b) where
  mapTermsM f (t1, t2) = do
    t1' <- mapTermsM f t1
    t2' <- mapTermsM f t2
    return (t1', t2')
  
instance Zip Alt' where
  zip (Alt' con bs t) (Alt' _ _ t') = Alt' con bs (t, t')
  
instance Zip Term' where
  zip (Var' x) (Var' _) = Var' x
  zip (App' f xs) (App' f' xs') = App' (f, f') (zip xs xs')
  zip (Lam' b t) (Lam' _ t') = Lam' b (t, t')
  zip (Fix' i b t) (Fix' _ _ t') = Fix' i b (t, t')
  zip (Con' con) (Con' {}) = Con' con
  zip (Unr' ty) (Unr' _) = Unr' ty
  zip (Case' t alts) (Case' t' alts') =
    Case' (t, t') (zipWith zip alts alts')

-- * Some generally helpful functions

app :: Term -> [Term] -> Term
app f [] = f
app (App f xs) ys = app f (xs ++ ys)
app f xs = App f xs

isCon :: Term -> Bool
isCon (Con {}) = True
isCon _ = False

isLam :: Term -> Bool
isLam (Lam {}) = True
isLam _ = False

isVar :: Term -> Bool
isVar (Var {}) = True
isVar _ = False

isFix :: Term -> Bool
isFix (Fix {}) = True
isFix _ = False

isCase :: Term -> Bool
isCase (Case {}) = True
isCase _ = False

isUnr :: Term -> Bool
isUnr (Unr {}) = True
isUnr _ = False

isUnr' :: Term' a -> Bool
isUnr' (Unr' {}) = True
isUnr' _ = False

fromVar :: Term -> Index
fromVar (Var x) = x

flattenLam :: Term -> ([Bind], Term)
flattenLam (Lam b t) = first (b:) (flattenLam t)
flattenLam t = ([], t)

unflattenLam :: [Bind] -> Term -> Term
unflattenLam = flip (foldr Lam)

flattenApp :: Term -> [Term]
flattenApp (App f xs) = f:xs
flattenApp t = [t]

leftmost :: Term -> Term
leftmost = head . flattenApp

arguments :: Term -> [Term]
arguments = tail . flattenApp

emptyInfo :: FixInfo
emptyInfo = FixInfo False Nothing Tag.omega

tags :: Term -> Set Tag
tags = Fold.cata ts
  where
  ts :: Term' (Set Tag) -> Set Tag
  ts (Fix' inf _ _) = Set.singleton (get fixTag inf)
  ts (Var' _) = Set.empty
  ts (Unr' _) = Set.empty
  ts (App' f xs) = Set.unions (f:xs)
  ts (Lam' _ t) =  t
  ts (Con' _) = Set.empty
  ts (Case' t alts) = 
    Set.unions (t : map (get altInner') alts)


-- | This should maybe be called @fullyApplied@ but it checks whether a fixpoint
-- has every argument applied to it.
inductivelyTyped :: Term -> Bool
inductivelyTyped (App (Fix _ (Bind _ fix_ty) _) args) =
  Type.argumentCount fix_ty == length args
  
  
  
-- | Given an inductive type and a constructor index this will return
-- a fully instantiated constructor term.
-- E.g. "altPattern [list] 1 == Cons _1 _0"
altPattern :: Constructor -> Term
altPattern con@(Constructor (Type.Ind _ cons) n) = id
  . assert (length cons > n)
  . app (Con con)
  . reverse
  . map (Var . enum)
  $ [0..arg_count - 1]
  where
  arg_count = id
    . length
    . snd
    $ cons `nth` fromEnum n
    
    
recursivePatternVars :: Constructor -> [Index]
recursivePatternVars con = 
  map (fromVar . (arguments (altPattern con) !!)) (Type.recursiveArgs con)
    
    
-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (Con {}) = True
isFinite (App (Con con) args) = id
  . all isFinite 
  . map (args `nth`)
  $ Type.recursiveArgs con
isFinite _ = False


-- | A /simple/ term contains only contructors and variables.
isSimple :: Term -> Bool
isSimple (flattenApp -> Con _ : args) =
  all isSimple args
isSimple (flattenApp -> Var _ : args) = 
  all isSimple args
isSimple _ = False


lowerableAltInner :: Indexed Term => Alt -> Bool
lowerableAltInner (Alt _ bs alt_t) =
  Indices.lowerableBy (length bs) alt_t

loweredAltInner :: Indexed Term => Alt -> Term
loweredAltInner (Alt _ bs alt_t) =
  Indices.lowerMany (length bs) alt_t
  
-- | Build a fold function. 
buildFold :: Indexed Term 
  => Type.Ind  -- ^ The inductive argument type of the fold function 
  -> Type      -- ^ The return type of the fold function
  -> Term
buildFold ind@(Type.Ind _ cons) result_ty = 
  unflattenLam lam_bs fix_t
  where
  -- Add a fixpoint if the inductive type is recursive
  fix_t 
    | not (Type.isRecursive ind) = Indices.lower fold_t
    | otherwise = Fix emptyInfo fix_b fold_t
    where
    fix_lbl = "fold[" ++ show ind ++ "]"
    fix_b = Bind fix_lbl (Fun (Base ind) result_ty)
  
  -- Build the term that represents the fold function body
  fold_t = id
    . Lam (Bind ("var_" ++ show ind) (Base ind))
    $ Case (Var 0) alts
    where
    -- Build every branch of the outer pattern match from the index of 
    -- the function which will be applied down that branch, and the
    -- defintion of the constructor for that branch
    alts = zipWith buildAlt lam_idxs [0..]
      where
      -- The indices of the functions which will replace each constructor
      lam_idxs :: [Index]
      lam_idxs = (map enum . reverse) [2..length cons + 1]
      
      buildAlt :: Index -> Nat -> Alt
      buildAlt f_idx con_n = 
        Alt con alt_bs (app f f_args)
        where
        (_, con_args) = cons !! con_n
        con = Constructor ind con_n
        liftHere = Indices.liftMany (length con_args)
        
        -- The index of the fix variable
        outer_f = liftHere (Var 1)
        
        -- The index of the parameter representing the function to be applied
        -- down this branch
        f = liftHere (Var f_idx)
        
        f_args = 
          zipWith conArgToArg arg_idxs con_args
          where
          arg_idxs :: [Index]
          arg_idxs = (map enum . reverse) [0..length con_args - 1]
          
          conArgToArg :: Index -> ConArg -> Term
          conArgToArg idx IndVar = app outer_f [Var idx]
          conArgToArg idx (ConArg _) = Var idx
        
        alt_bs = 
          map conArgToBind con_args
          where
          conArgToBind IndVar = Bind "y" (Base ind)
          conArgToBind (ConArg ty) = Bind "a" ty
    
  -- The bindings for the outer lambdas of the fold function. 
  -- These are the lambdas which receive the functions the constructors get 
  -- replaced with when folding.
  lam_bs = 
    map makeBind cons
    where
    -- Turn a inductive constructor definition into the type of
    -- the corresponding fold parameter. So for nat lists you'd get,
    -- where X is 'result_ty':
    -- ("Nil", []) => X
    -- ("Cons", [ConArg nat, IndVar]) => nat -> X -> X
    makeBind :: (String, [ConArg]) -> Bind
    makeBind (name, conargs) = id
      . Bind ("case_" ++ name)
      . Type.unflatten
      $ map conArgToTy conargs ++ [result_ty]
      where
      conArgToTy IndVar = result_ty
      conArgToTy (ConArg ty) = ty
   
    
false, true :: Term
true = Con Type.true
false = Con Type.false
      
-- | Returns an n-argument /and/ term.
-- > conjunction 3 = fun (p q r: bool) -> and p (and q r)
conjunction :: Indexed Term => Int -> Term
conjunction n = id
  . unflattenLam (replicate n (Bind "p" (Base Type.bool)))
  . foldr and true
  . reverse
  $ map (Var . enum) [0..n-1]
  where
  and t t' = app bool_fold [t', false, t]
  bool_fold = buildFold Type.bool (Base Type.bool)

    
-- | Build an equality function for a given inductive type.
-- > buildEq nat : nat -> nat -> bool
buildEq :: (Indexed Term, Show Term) => Type.Ind -> Term
buildEq ind@(Ind _ cons) = 
  app (buildFold ind fold_ty) cases
  where
  fold_ty = Fun (Base ind) (Base Type.bool)
  cases = map buildCase [0..length cons - 1]
  
  buildCase :: Int -> Term
  buildCase case_n = id 
    . unflattenLam arg_bs
    $ Case (Var 0) alts
    where
    (_, con_args) = cons `nth` case_n
    
    arg_bs = id
      . map (Bind "y")
      $ map getArgTy con_args ++ [Base ind]
      where
      getArgTy IndVar = fold_ty
      getArgTy (ConArg ty) = ty

    alts = map makeAlt [0..length cons - 1]
      where 
      makeAlt :: Int -> Alt
      makeAlt match_n = 
        Alt con alt_bs alt_t
        where
        con = Constructor ind (enum match_n)
        match_con_args = snd (cons `nth` match_n)
        
        alt_bs = map getArgBs match_con_args
          where
          getArgBs IndVar = Bind "r" (Base ind)
          getArgBs (ConArg ty) = Bind "a" ty
        
        alt_t 
          | case_n /= match_n = false
          | otherwise = app (conjunction (length eq_checks)) eq_checks 
          
        eq_checks = 
          zipWith getEqCheck (reverse [0..length con_args - 1]) con_args
          where
          getEqCheck _ (ConArg (Fun {})) = 
            error 
              $ "Cannot build an equality function for an inductive type "
              ++ "which has function typed constructor arguments."
          getEqCheck arg_i (ConArg (Base arg_ind)) = 
            app eq_fun [Var (enum arg_i'), Var (enum arg_i)]
            where
            arg_i' = arg_i + 1 + length con_args
            eq_fun = buildEq arg_ind
          getEqCheck arg_i IndVar = 
            app f_var [Var (enum arg_i)]
            where
            f_var = Var (enum (arg_i + 1 + length con_args))

