module Elea.Term
(
  module Elea.Term.Index,
  Tag, Tagged (..),
  Term (..), Alt (..),
  Match (..), Pattern (..), Constraint,
  Term' (..), Alt' (..),
  Type, Bind (..), Constructor (..),
  FixInfo (..),
  ContainsTerms (..), mapTerms, containedTerms,
  Equation (..), equationName, equationTerm,
  app,
  projectAlt, embedAlt,
  altBindings, altInner, altConstructor,
  altBindings', altInner', altConstructor',
  matchTerm, matchPatterns, matchIndex, 
  matchedPattern, matchedTo, matchedTerm,
  matchFromCase,
  patternConstructor, patternBindings, patternVars,
  fixIndex, fixDomain,
  flattenApp, leftmost, arguments,
  flattenLam, unflattenLam, unflattenApp,
  isCon, isLam, isVar,
  isFix, isBot, isCase, 
  isEql, isQuantifiedEql,
  emptyInfo,
  inductivelyTyped, 
  fromVar, 
  and, conjunction, true, false,
  altPattern, recursivePatternVars,
  patternTerm,
  isSimple,
  isFinite,
  lowerableAltInner,
  loweredAltInner,
  buildFold,
  buildEq,
  stripTags,
  beingFused,
  recursiveId,
  isFixPromoted,
)
where

import Elea.Prelude hiding ( and )
import Elea.Term.Index ( Index, Indexed, Substitutable, Inner )
import Elea.Type ( Type (..), Ind (..), ConArg (..)
                 , Bind (..), Constructor (..) )
import Elea.Term.Tag ( Tag, Tagged (..) )
import Elea.Embed ( Encodable (..), Atom (..), cantor )
import qualified Elea.Type as Type
import qualified Elea.Embed as Embed
import qualified Elea.Term.Index as Indices
import qualified Elea.Term.Tag as Tag
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Algebra.Lattice as Algebra


data Term
  = Eql     { eqLeft :: !Term
            , eqRight :: !Term } 
            
  | Bot     { botType :: !Type }
  
  | Var     { varIndex :: !Index }

  | App     !Term ![Term]

  | Lam     { binding :: !Bind 
            , inner :: !Term }

  | Fix     { fixInfo :: !FixInfo
            , binding :: !Bind 
            , inner :: !Term }

  | Con     { constructor :: !(Tagged Constructor) }

  | Case    { caseOf :: !Term
            , caseAlts :: ![Alt] }
  deriving ( Eq, Ord )

  
data Alt
  = Alt     { _altConstructor :: !(Tagged Constructor)
            , _altBindings :: ![Bind]
            , _altInner :: !Term }
  deriving ( Eq, Ord )
  
 
data Match
  = Match   { _matchTerm :: !Term
            , _matchPatterns :: ![Pattern]
            , _matchIndex :: !Nat }
            
data Pattern
  = Pattern { _patternConstructor :: !(Tagged Constructor)
            , _patternBindings :: ![Bind] 
            , _patternVars :: ![Index] }
  deriving ( Eq, Ord )
  
-- | A constraint is a pattern match over a non-variable term.
-- It is intended to be used in the context of constraining the domain
-- of a fixed-point. See 'FixInfo' and 'Elea.Term.Constraint'.
type Constraint = Match
            
-- | Information stored about fixpoints, to add efficiency.
data FixInfo
  = FixInfo { _fixDomain :: !(Set Constraint)
            , _fixIndex :: !Tag }
            
              
-- Fixpoint information does not change the meaning of a fixpoint, so
-- it does not effect equality between terms.
instance Eq FixInfo where
  (==) = (==) `on` _fixIndex
  
instance Ord FixInfo where
  compare = compare `on` _fixIndex
  
instance Eq Match where
  Match t _ n == Match t' _ n' =
    n == n' && t == t'
    
instance Ord Match where
  Match t _ n `compare` Match t' _ n' = 
    n `compare` n' ++ t `compare` t'
  
-- | Equations between terms
data Equation
  = Equals  { _equationName :: String
            , _equationVars :: [Bind]
            , _equationTerm :: Term }

 
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Eql' a a
  | Bot' !Type
  | Var' !Index
  | App' a [a]
  | Lam' !Bind a
  | Fix' !FixInfo !Bind a
  | Con' !(Tagged Constructor)
  | Case' a ![Alt' a]
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altConstructor' :: !(Tagged Constructor)
         , _altBindings' :: ![Bind]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
  
mkLabels [ ''Alt, ''Alt', ''Equation, ''Pattern, ''Match, ''FixInfo ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt c bs t) = Alt' c bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' c bs t) = Alt c bs t


instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App f xs) = App' f xs
  project (Lam b t) = Lam' b t
  project (Fix i b t) = Fix' i b t
  project (Con tc) = Con' tc
  project (Case t alts) = Case' t (map projectAlt alts)
  project (Eql t t') = Eql' t t'
  project (Bot ty) = Bot' ty

instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' f xs) = App f xs
  embed (Lam' b t) = Lam b t
  embed (Fix' i b t) = Fix i b t
  embed (Con' tc) = Con tc
  embed (Case' t alts) = Case t (map embedAlt alts)
  embed (Eql' t t') = Eql t t'
  embed (Bot' ty) = Bot ty
  
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
  mapTermsM f = modifyM equationTerm f
  
instance (ContainsTerms a, ContainsTerms b) => ContainsTerms (a, b) where
  mapTermsM f (t1, t2) = do
    t1' <- mapTermsM f t1
    t2' <- mapTermsM f t2
    return (t1', t2')
    
instance ContainsTerms a => ContainsTerms (Maybe a) where
  mapTermsM _ Nothing = return Nothing
  mapTermsM f (Just t) = return Just `ap` mapTermsM f t
  
  
instance Zip Alt' where
  zip (Alt' con bs t) (Alt' _ _ t') = Alt' con bs (t, t')
  
instance Zip Term' where
  zip (Var' x) (Var' _) = Var' x
  zip (App' f xs) (App' f' xs') = App' (f, f') (zip xs xs')
  zip (Lam' b t) (Lam' _ t') = Lam' b (t, t')
  zip (Fix' i b t) (Fix' _ _ t') = Fix' i b (t, t')
  zip (Con' con) (Con' {}) = Con' con
  zip (Eql' x y) (Eql' x' y') = Eql' (x, x') (y, y')
  zip (Bot' ty) (Bot' _) = Bot' ty
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

isEql :: Term -> Bool
isEql (Eql _ _) = True
isEql _ = False

isQuantifiedEql :: Term -> Bool
isQuantifiedEql = 
  isEql . snd . flattenLam

isCase :: Term -> Bool
isCase (Case {}) = True
isCase _ = False
  
isBot :: Term -> Bool
isBot (Bot _) = True
isBot _ = False

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

unflattenApp :: [Term] -> Term
unflattenApp (f:xs) = app f xs

leftmost :: Term -> Term
leftmost = head . flattenApp

arguments :: Term -> [Term]
arguments = tail . flattenApp

emptyInfo :: FixInfo
emptyInfo = FixInfo Set.empty Tag.omega

  
-- | This should maybe be called @fullyApplied@ but it checks whether a fixpoint
-- has every argument applied to it.
inductivelyTyped :: Term -> Bool
inductivelyTyped (App (Fix _ (Bind _ fix_ty) _) args) =
  Type.argumentCount fix_ty == length args
  

-- | If a fixed-point is in the process of being fused into a context.
-- Can be detected as the tag will not be omega
beingFused :: Term -> Bool
beingFused (Fix i _ _) =
  get fixIndex i /= Tag.omega

  
matchedPattern :: Match -> Pattern
matchedPattern (Match _ pts n) = pts !! n

matchedTerm :: Match -> Term
matchedTerm = get matchTerm
  
matchFromCase :: Indexed Term => Nat -> Term -> Match
matchFromCase n (Case cse_t alts) =
  Match match_t (map altPattern alts) n
  where
  match_t = Indices.liftMany (length bs) cse_t
  Alt _ bs _ = alts !! n
  pats = zipWith stripVars [0..] (map altPattern alts)
  
  stripVars :: Int -> Pattern -> Pattern
  stripVars i p@(Pattern tc bs _)
    | i == enum n = p
    | otherwise = Pattern tc bs []
    
matchedTo :: Match -> Term
matchedTo = patternTerm . matchedPattern
  
  
altPattern :: Alt -> Pattern
altPattern (Alt tcon bs _) =
  Pattern tcon bs vars
  where
  Constructor (Type.Ind _ cons) n = Tag.tagged tcon
  vars = (reverse . map enum) [0..arg_count-1]
  arg_count = (length . snd . (cons !!)) n
    
patternTerm :: Pattern -> Term
patternTerm (Pattern tcon _ xs) =
  app (Con tcon) (map Var xs)
  
instance Indexed Pattern where
  free = Set.fromList . get patternVars
  shift f = modify patternVars (map f)
  
recursivePatternVars :: Constructor -> [Index]
recursivePatternVars con = id
  . map enum
  . invert
  $ Type.recursiveArgs con
    
    
-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (Con {}) = True
isFinite (App (Con tcon) args) = id
  . all isFinite 
  . map (args !!)
  . Type.recursiveArgs 
  $ Tag.tagged tcon
isFinite _ = False


-- | A /simple/ term contains only contructors and variables.
isSimple :: Term -> Bool
isSimple (flattenApp -> Con {} : args) =
  all isSimple args
isSimple (flattenApp -> Var {} : args) = 
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
        con = Tag.with Tag.null (Constructor ind con_n)
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

      
-- | Returns the recursive identity function for a given type      
recursiveId :: Indexed Term => Type.Ind -> Term
recursiveId ind@(Ind _ cons) = 
  app id_fold fold_cons
  where
  id_fold = buildFold ind (Type.Base ind)
  fold_cons = map (Con . Tag.with Tag.null) (Type.constructors ind)
   
    
false, true :: Term
true = Con (Tag.with Tag.null Type.true)
false = Con (Tag.with Tag.null Type.false)
      
and :: Indexed Term => Term -> Term -> Term
and t t' = app bool_fold [t', false, t]
  where
  bool_fold = buildFold Type.bool (Base Type.bool)

-- | Returns an n-argument /and/ term.
-- > conjunction 3 = fun (p q r: bool) -> and p (and q r)
conjunction :: Indexed Term => Nat -> Term
conjunction n = id
  . unflattenLam (replicate (enum n) (Bind "p" (Base Type.bool)))
  . foldr and true
  . reverse
  $ map (Var . enum) [0..n-1]
  
    
-- | Build an equality function for a given inductive type.
-- > buildEq nat : nat -> nat -> bool
buildEq :: (Indexed Term, Show Term) => Type.Ind -> Term
buildEq ind@(Ind _ cons) = 
  app (buildFold ind fold_ty) cases
  where
  fold_ty = Fun (Base ind) (Base Type.bool)
  cases = map buildCase [0..length cons - 1]
  
  buildCase :: Nat -> Term
  buildCase case_n = id 
    . unflattenLam arg_bs
    $ Case (Var 0) alts
    where
    (_, con_args) = cons !! case_n
    
    arg_bs = id
      . map (Bind "y")
      $ map getArgTy con_args ++ [Base ind]
      where
      getArgTy IndVar = fold_ty
      getArgTy (ConArg ty) = ty

    alts = map makeAlt [0..length cons - 1]
      where 
      makeAlt :: Nat -> Alt
      makeAlt match_n = 
        Alt con alt_bs alt_t
        where
        con = Tag.with Tag.null (Constructor ind (enum match_n))
        match_con_args = snd (cons !! match_n)
        
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
  

-- | Use this to strip all tags and thereby reduce indexed fixed-points
-- to fixed-points. Useful if you want to check term equality modulo tags.
stripTags :: Term -> Term
stripTags = Fold.transform strip
  where
  strip (Fix inf b t) = 
    Fix (set fixIndex Tag.omega inf) b t
  strip other = other
  

isFixPromoted :: Indexed Term => Term -> Bool
isFixPromoted (App fix@(Fix {}) xs) =
  all isVar xs 
  && elength x_vars == Set.size x_vars_set
  && Set.null overlap
  where
  x_vars = map fromVar xs
  x_vars_set = Set.fromList x_vars
  overlap = Set.intersection (Indices.free fix) x_vars_set
isFixPromoted _ = False

  

instance Encodable Term where
  encodeRaw (Bot _) = [cantor (8, 0)]
  encodeRaw (Eql x y) = []
    ++ [cantor (1, 0)] 
    ++ encodeRaw x 
    ++ [cantor (1, 0)] 
    ++ encodeRaw y
  encodeRaw (Var _) = [cantor (2, 0)]
  encodeRaw (Lam _ t) = []
    ++ [cantor (3, 0)] 
    ++ encodeRaw t
  encodeRaw (Con tcon) = [atom tcon]
  encodeRaw (App f xs) = []
    ++ encodeRaw f 
    ++ concatMap (([cantor (4, 0)] ++) . encodeRaw) xs
  encodeRaw (Fix fix_i _ fix_t) = []
    ++ [atom fix_i]
    ++ encodeRaw fix_t
  encodeRaw (Case cse_t alts) = []
    ++ [cantor (9, 0)]
    ++ encodeRaw cse_t 
    ++ concatMap encodeRaw alts 
    ++ [cantor (9, 1)]
    
instance Atom Constructor where
  atom (Constructor ind idx) = 
    atom [20, atom (get Type.indName ind), atom idx]
    
    
instance Encodable Alt where
  encodeRaw (Alt tcon bs t) = []
    ++ [atom tcon]
    ++ encodeRaw t
    
instance Atom FixInfo where
  atom (FixInfo _ idx)
    | idx == Tag.omega = cantor (5, 0)
    | otherwise = cantor (5, 1)
      

