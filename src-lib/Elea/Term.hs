module Elea.Term
(
  module Elea.Index,
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Type, Bind (..), 
  Constraint (..), FixInfo (..),
  ContainsTerms (..), mapTerms, containedTerms,
  Equation (..), equationName, equationLHS, equationRHS,
  apply, stripArgs, argumentCount,
  projectAlt, embedAlt,
  altBindings, altTerm,
  altBindings', altTerm',
  constrainedTo, constrainedTerm,
  flattenLam, unflattenLam,
  isCon, isLam, isVar,
  isFix, isUnr, isCase,
  isUnr',
  emptyInfo,
  fixName, fixFailedConstraints, fixClosed,
  addFusedMatches,
  inductivelyTyped,
  constructorPattern,
  isSimple,
  isFinite,
  lowerableAltInner,
  loweredAltInner,
)
where

import Elea.Prelude
import Elea.Index ( Index, Indexed, Substitutable, Inner )
import Elea.Type ( Type (..), Ind (..), ConArg (..)
                 , Bind (..), Constructor (..) )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | The simply typed lambda calculus
data Term
  = Unr     { resultType :: !Type } 
  
  | Var     { varIndex :: !Index
            , arguments :: ![Term] }

  | Lam     { binding :: !Bind 
            , abstractedTerm :: !Term }

  | Fix     { fixInfo :: !FixInfo
            , binding :: !Bind
            , fixBody :: !Term 
            , arguments :: ![Term] }

  | Con     { constructor :: !Constructor
            , arguments :: ![Term] }

  | Case    { caseOf :: !Term
            , caseAlts :: ![Alt] }
  deriving ( Eq, Ord )

data Alt
  = Alt     { _altConstructor :: !Constructor
            , _altBindings :: ![Bind]
            , _altTerm :: !Term }
  deriving ( Eq, Ord )
  
-- | A constraint is a pattern match where only one branch is non-absurd.
-- Functions surrounding constraints can be found in "Elea.Constraint".
data Constraint 
  = Constraint  { _constrainedTo :: !Constructor
                , _constrainedTerm :: !Term }
  deriving ( Eq, Ord )
  
  
-- | Information stored about fixpoints, to add efficiency.
data FixInfo
  = FixInfo { -- | The sets of pattern matches that have been unsuccessfully
              -- fused into this fixpoint, and the value of the applied
              -- arguments to the fixpoint itself.
              -- Stops match-fix fusion from repeating itself.
              _fixFailedConstraints :: ![(Set Constraint, Term)] 
              
             -- | True if this fixpoint contains no free variables.
            , _fixClosed :: !Bool
            
            -- | A shorthand name for this fixpoint, usually taken from
            -- the source code it was defined in, like "reverse" for the 
            -- reverse function.
            , _fixName :: Maybe String }
              

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
  | Var' !Index [a]
  | Lam' !Bind a
  | Fix' !FixInfo !Bind a [a]
  | Con' !Constructor [a]
  | Case' a ![Alt' a]
  deriving ( Functor, Foldable, Traversable )
  
data Alt' a
  = Alt' { _altConstructor' :: !Constructor
         , _altBindings' :: ![Bind]
         , _altTerm' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Alt, ''Alt', ''FixInfo, ''Equation, ''Constraint ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt c bs t) = Alt' c bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' c bs t) = Alt c bs t

instance Fold.Foldable Term where
  project (Var x ts) = Var' x ts
  project (Lam b t) = Lam' b t
  project (Fix i b t ts) = Fix' i b t ts
  project (Con c ts) = Con' c ts
  project (Case t alts) = Case' t (map projectAlt alts)
  project (Unr ty) = Unr' ty

instance Fold.Unfoldable Term where
  embed (Var' x ts) = Var x ts
  embed (Lam' b t) = Lam b t
  embed (Fix' i b t ts) = Fix i b t ts
  embed (Con' c ts) = Con c ts
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
  zip (Alt' c bs t) (Alt' _ _ t') = Alt' c bs (t, t')
  
instance Zip Term' where
  zip (Var' x ts) (Var' _ ts') = Var' x (zip ts ts')
  zip (Lam' b t) (Lam' _ t') = Lam' b (t, t')
  zip (Fix' i b t ts) (Fix' _ _ t' ts') = Fix' i b (t, t') (zip ts ts')
  zip (Con' c ts) (Con' _ ts') = Con' c (zip ts ts')
  zip (Unr' ty) (Unr' _) = Unr' ty
  zip (Case' t alts) (Case' t' alts') =
    Case' (t, t') (zipWith zip alts alts')
  

-- * Some generally helpful functions

-- | Apply arguments to the given term. Only defined if the term given
-- takes arguments, or if no arguments are given.
apply :: (Substitutable Term, Inner Term ~ Term) => Term -> [Term] -> Term
apply t [] = t
-- Beta-reduction built in
apply (Lam _ t) (x:xs) = apply (Indices.substAt 0 x t) xs
apply (Var f xs) ys = Var f (xs ++ ys)
apply (Fix i b t xs) ys = Fix i b t (xs ++ ys)
apply (Con c xs) ys = Con c (xs ++ ys)

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

flattenLam :: Term -> ([Bind], Term)
flattenLam (Lam b t) = first (b:) (flattenLam t)
flattenLam t = ([], t)

unflattenLam :: [Bind] -> Term -> Term
unflattenLam = flip (foldr Lam)

emptyInfo :: FixInfo
emptyInfo = FixInfo [] False Nothing

stripArgs :: Fail.Can m => Nat -> Term -> m (Term, [Term])
stripArgs 0 t = return (t, [])
stripArgs n t
  | argumentCount t < n = Fail.here
stripArgs n (Var x ts) = return (Var x (drop n ts), take n ts)
stripArgs n (Fix i b t ts) = return (Fix i b t (drop n ts), take n ts)
stripArgs n (Con c ts) = return (Con c (drop n ts), take n ts)

-- | Returns zero if the given term takes no arguments.
argumentCount :: Term -> Nat
argumentCount (Var _ ts) = length ts
argumentCount (Fix _ _ _ ts) = length ts
argumentCount (Con _ ts) = length ts
argumentCount _ = 0


-- | This should maybe be called @fullyApplied@ but it checks whether a fixpoint
-- has every argument applied to it.
inductivelyTyped :: Term -> Bool
inductivelyTyped (Fix _ (Bind _ fix_ty) _ args) =
  Type.argumentCount fix_ty == length args
  
  
-- | Add a set of matches we attempted to fuse into a fixpoint.
addFusedMatches :: Set Constraint -> Term -> Term
addFusedMatches ms fix@(Fix {}) = 
  fix { fixInfo = info' }
  where
  info' = modify fixFailedConstraints (++ [(ms, fix)]) (fixInfo fix)
  
  
-- | Given a type constructor this will return a fully instantiated 
-- constructor term as it would appear in a pattern match.
-- E.g. @altPattern ([list], 1) == Cons _1 _0@
constructorPattern :: Constructor -> Term
constructorPattern con@(Constructor (Ind _ cons) n) = id
  . assert (length cons > n)
  . Con con
  . reverse
  . map (\i -> Var i [])
  $ [0..arg_count - 1]
  where
  arg_count = id
    . length
    . snd
    $ cons !! n
    
    
-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (Con c args) = id
  . all isFinite 
  . map (args `nth`)
  $ Type.recursiveArgs c
isFinite _ = False


-- | A /simple/ term contains only contructors and variables.
isSimple :: Term -> Bool
isSimple (Con _ args) =
  all isSimple args
isSimple (Var _ args) = 
  all isSimple args
isSimple _ = False


lowerableAltInner :: Indexed Term => Alt -> Bool
lowerableAltInner (Alt _ bs alt_t) =
  Indices.lowerableBy (length bs) alt_t

loweredAltInner :: Indexed Term => Alt -> Term
loweredAltInner (Alt _ bs alt_t) =
  Indices.lowerMany (length bs) alt_t
  

