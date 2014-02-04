module Elea.Term
(
  module Elea.Index,
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Type, Bind (..), FixInfo (..),
  ContainsTerms (..), mapTerms,
  Equation (..), equationName, equationLHS, equationRHS,
  app,
  projectAlt, embedAlt,
  altBindings, altInner,
  altBindings', altInner',
  fusedMatches,
  flattenApp, leftmost, arguments,
  flattenLam, unflattenLam,
  isCon, isLam, isVar,
  isFix, isAbsurd, isCase,
  isAbsurd',
  fromVar, 
  matchedTo,
  altPattern, 
  isFinite,
  buildFold,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index ( Index, Indexed, Substitutable, Inner )
import Elea.Type ( Type, Ind, ConArg, Bind (..) )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | The simply typed lambda calculus
data Term
  = Var     { varIndex :: !Index }

  | App     !Term ![Term]

  | Lam     { binding :: !Bind 
            , inner :: !Term }

  | Fix     { fixInfo :: !FixInfo
            , binding :: !Bind 
            , inner :: !Term }

  | Con     { inductiveType :: !Type.Ind
            , constructorIndex :: !Nat }

  | Case    { inductiveType :: !Type.Ind
            , inner :: !Term
            , alts :: ![Alt] }

  | Absurd  { resultType :: !Type }
  deriving ( Eq, Ord )

data Alt
  = Alt     { _altBindings :: ![Bind]
            , _altInner :: !Term }
  deriving ( Eq, Ord )
  
-- | Information stored about fixpoints, to add efficiency.
data FixInfo
  = FixInfo { -- | The pattern matches that have been fused into this fixpoint.
              -- We only store the constructor index of the matched pattern
              -- since the type can be inferred from the term, and the matched
              -- variables are always fresh.
              _fusedMatches :: ![(Term, Nat)] }
              
-- Fixpoint information does not change the meaning of a fixpoint, so
-- it does not effect equality between terms.
instance Eq FixInfo where
  _ == _ = True
instance Ord FixInfo where
  _ `compare` _ = EQ
  
instance Monoid FixInfo where
  mempty = FixInfo []
  
  -- Map union is what we are going for here.
  -- We only store these as lists rather than map
  -- to allow easier definitions later.
  FixInfo i1 `mappend` FixInfo i2 = 
    FixInfo (Map.toList (Map.fromList i1 ++ Map.fromList i2))
  
-- | Equations between terms
data Equation
  = Equals  { _equationName :: String
            , _equationVars :: [Bind]
            , _equationLHS :: Term
            , _equationRHS :: Term }
 
-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Var' !Index
  | App' a [a]
  | Lam' !Bind a
  | Fix' !FixInfo !Bind a
  | Con' !Type.Ind !Nat
  | Case' !Type.Ind a ![Alt' a]
  | Absurd' !Type
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altBindings' :: ![Bind]
         , _altInner' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Alt, ''Alt', ''FixInfo, ''Equation ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt bs t) = Alt' bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' bs t) = Alt bs t

instance Fold.Foldable Term where
  project (Var x) = Var' x
  project (App f xs) = App' f xs
  project (Lam b t) = Lam' b t
  project (Fix i b t) = Fix' i b t
  project (Con ind n) = Con' ind n
  project (Case ind t alts) = Case' ind t (map projectAlt alts)
  project (Absurd ty) = Absurd' ty

instance Fold.Unfoldable Term where
  embed (Var' x) = Var x
  embed (App' f xs) = App f xs
  embed (Lam' b t) = Lam b t
  embed (Fix' i b t) = Fix i b t
  embed (Con' ind n) = Con ind n
  embed (Case' ind t alts) = Case ind t (map embedAlt alts)
  embed (Absurd' ty) = Absurd ty
  
class ContainsTerms t where
  mapTermsM :: Monad m => (Term -> m Term) -> t -> m t
  
mapTerms :: ContainsTerms t => (Term -> Term) -> t -> t
mapTerms f = runIdentity . mapTermsM (return . f)

instance ContainsTerms Equation where
  mapTermsM f = modifyM equationLHS f <=< modifyM equationRHS f
  
instance Zip Alt' where
  zip (Alt' bs t) (Alt' _ t') = Alt' bs (t, t')
  
instance Zip Term' where
  zip (Var' x) (Var' _) = Var' x
  zip (App' f xs) (App' f' xs') = App' (f, f') (zip xs xs')
  zip (Lam' b t) (Lam' _ t') = Lam' b (t, t')
  zip (Fix' i b t) (Fix' _ _ t') = Fix' i b (t, t')
  zip (Con' ind n) (Con' {}) = Con' ind n
  zip (Absurd' ty) (Absurd' _) = Absurd' ty
  zip (Case' ind t alts) (Case' _ t' alts') =
    Case' ind (t, t') (zipWith zip alts alts')
  

-- * Some generally helpful functions

app :: Term -> [Term] -> Term
app f [] = f
app (App f xs) ys = App f (xs ++ ys)
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

isAbsurd :: Term -> Bool
isAbsurd (Absurd {}) = True
isAbsurd _ = False

isAbsurd' :: Term' a -> Bool
isAbsurd' (Absurd' {}) = True
isAbsurd' _ = False

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

matchedTo :: FixInfo -> Term -> Maybe Nat
matchedTo (FixInfo fused) term = 
  lookup term fused
  
-- | Given an inductive type and a constructor index this will return
-- a fully instantiated constructor term.
-- E.g. "altPattern [list] 1 == Cons _1 _0"
altPattern :: Type.Ind -> Nat -> Term
altPattern ty@(Type.Ind _ cons) n = id
  . app (Con ty n)
  . reverse
  . map (Var . enum)
  $ [0..arg_count - 1]
  where
  arg_count = id
    . length
    . snd
    $ cons !! fromEnum n
    
-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (Con {}) = True
isFinite (App (Con ind n) args) = id
  . all isFinite 
  . map (args !!)
  $ Type.recursiveArgs ind n
isFinite _ = False
  

-- | Build a term representing a catamorphism (fold function).
buildFold :: Indexed Term 
  => Type.Ind  -- ^ The inductive type the catamorphism 
  -> Type      -- ^ The return type of the catamorphism
  -> Term
buildFold ind@(Type.Ind _ cons) result_ty = 
  unflattenLam lam_bs fix
  where
  -- Build the fixpoint that represents the fold function.
  fix = id
    . Fix mempty fix_b
    . Lam (Bind ("var_" ++ show ind) (Type.Base ind))
    $ Case ind (Var 0) alts
    where
    fix_lbl = "fold[" ++ show ind ++ "]"
    fix_b = Bind fix_lbl (Type.Fun (Type.Base ind) result_ty)
    
    -- Build every branch of the outer pattern match from the index of 
    -- the function which will be applied down that branch, and the
    -- defintion of the constructor for that branch
    alts = zipWith buildAlt lam_idxs cons
      where
      -- The indices of the functions which will replace each constructor
      lam_idxs :: [Index]
      lam_idxs = (map enum . reverse) [2..length cons + 1]
      
      buildAlt :: Index -> (String, [Type.ConArg]) -> Alt
      buildAlt f_idx (_, con_args) = 
        Alt alt_bs (app f f_args)
        where
        liftHere = Indices.liftMany (elength con_args)
        
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
          
          conArgToArg :: Index -> Type.ConArg -> Term
          conArgToArg idx Type.IndVar = app outer_f [Var idx]
          conArgToArg idx (Type.ConArg _) = Var idx
        
        alt_bs = 
          map conArgToBind con_args
          where
          conArgToBind Type.IndVar = Bind "y" (Type.Base ind)
          conArgToBind (Type.ConArg ty) = Bind "a" ty
    
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
    makeBind :: (String, [Type.ConArg]) -> Bind
    makeBind (name, conargs) = id
      . Bind ("case_" ++ name)
      . Type.unflatten
      $ map conArgToTy conargs ++ [result_ty]
      where
      conArgToTy Type.IndVar = result_ty
      conArgToTy (Type.ConArg ty) = ty
    

