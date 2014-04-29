module Elea.Term
(
  module Elea.Index,
  
  Term (..), Alt (..),
  Term' (..), Alt' (..),
  Type, Bind (..),
  Constraint (..),
  Name (..),
  
  ContainsTerms (..), mapTerms, containedTerms,
  Equation (..), equationName, equationLHS, equationRHS,
  
  projectAlt, embedAlt,
  altBindings, altTerm, altConstructor,
  altBindings', altTerm', altConstructor',
  recursiveConArgs,
  constrainedTerm, constrainedTo,
  flattenLam, unflattenLam,
  isCon, isLam, isVar, isUnr, isCase,
  isUnr',
  true, false,
  constructorPattern,
  argumentCount,
  apply,
  stripArgs,
  isSimple,
  isFinite,
  lowerableAltTerm,
  loweredAltTerm,
  stripPrefix,
)
where

import Elea.Prelude
import Elea.Name ( Name )
import Elea.Index ( Index, Indexed, Substitutable, Inner )
import Elea.Type ( Type, Ind (..), ConArg (..)
                 , Bind (..), Constructor (..) )
import qualified Elea.Type as Type
import qualified Elea.Name as Name
import qualified Elea.Index as Indices
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List


data Term
  = Var     { varIndex :: !Index
            , arguments :: ![Term] }

  | Lam     { binding :: !Bind 
            , abstractedTerm :: !Term }

  | Def     { name :: !Name
            , arguments :: ![Term] }
            
  | Con     { constructor :: !Constructor
            , arguments :: ![Term] }

  | Case    { caseTerm :: !Term
            , caseAlts :: ![Alt] }
            
  | Unr     { resultType :: !Type } 
            
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
  
  
-- | Equations between terms
data Equation
  = Equals  { _equationName :: String
            , _equationVars :: [Bind]
            , _equationLHS :: Term
            , _equationRHS :: Term }


-- * Base types for generalised cata/para morphisms.
  
type instance Fold.Base Term = Term'

data Term' a 
  = Var'    { varIndex' :: !Index
            , arguments' :: [a] }

  | Lam'    { binding' :: !Bind 
            , abstractedTerm' :: a }

  | Def'    { name' :: !Name
            , arguments' :: [a] }
            
  | Con'    { constructor' :: !Constructor
            , arguments' :: [a] }

  | Case'   { caseTerm' :: a
            , caseAlts' :: [Alt' a] }
            
  | Unr'    { resultType' :: !Type } 
  deriving ( Functor, Foldable, Traversable )
  
  
data Alt' a
  = Alt'    { _altConstructor' :: !Constructor
            , _altBindings' :: ![Bind]
            , _altTerm' :: a }
  deriving ( Functor, Foldable, Traversable )
  
mkLabels [ ''Alt, ''Alt', ''Equation, ''Constraint ]

projectAlt :: Alt -> Alt' Term
projectAlt (Alt c bs t) = Alt' c bs t

embedAlt :: Alt' Term -> Alt
embedAlt (Alt' c bs t) = Alt c bs t

instance Fold.Foldable Term where
  project (Var x ts) = Var' x ts
  project (Lam b t) = Lam' b t
  project (Def n t) = Def' n t
  project (Con c xs) = Con' c xs
  project (Case t alts) = Case' t (map projectAlt alts)
  project (Unr ty) = Unr' ty

instance Fold.Unfoldable Term where
  embed (Var' x ts) = Var x ts
  embed (Lam' b t) = Lam b t
  embed (Def' n ts) = Def n ts
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
  zip (Var' f ts) (Var' _ ts') = Var' f (zip ts ts')
  zip (Lam' b t) (Lam' _ t') = Lam' b (t, t')
  zip (Def' n ts) (Def' _ ts') = Def' n (zip ts ts')
  zip (Con' c ts) (Con' _ ts') = Con' c (zip ts ts')
  zip (Unr' ty) (Unr' _) = Unr' ty
  zip (Case' t alts) (Case' t' alts') =
    Case' (t, t') (zipWith zip alts alts')
  

-- * Some generally helpful functions

isCon :: Term -> Bool
isCon (Con {}) = True
isCon _ = False

isLam :: Term -> Bool
isLam (Lam {}) = True
isLam _ = False

isVar :: Term -> Bool
isVar (Var {}) = True
isVar _ = False

isDef :: Term -> Bool
isDef (Def {}) = True
isDef _ = False

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

-- | Apply arguments to the given term. Only defined if the term given
-- takes arguments, or if no arguments are given.
apply :: (Substitutable Term, Inner Term ~ Term) => Term -> [Term] -> Term
apply t [] = t
-- Beta-reduction built in
apply (Lam _ t) (x:xs) = apply (Indices.substAt 0 x t) xs
apply (Var f xs) ys = Var f (xs ++ ys)
apply (Def a xs) ys = Def a (xs ++ ys)
apply (Con c xs) ys = Con c (xs ++ ys)


-- | Returns the arguments that can be applied to the first term
-- to yield the second.
-- More formally:
-- > Just xs = stripPrefix t t' <=> apply t xs == t'
stripPrefix :: Fail.Can m => Term -> Term -> m [Term]
stripPrefix t1 t2 
  | t1 == t2 = return []
stripPrefix (Def n1 xs1) (Def n2 xs2) 
  | n1 == n2, xs1 `isPrefixOf` xs2 =
    return (drop (nlength xs1) xs2)
stripPrefix (Con c1 xs1) (Con c2 xs2)
  | c1 == c2, xs1 `isPrefixOf` xs2 =
    return (drop (nlength xs1) xs2)
stripPrefix (Var x1 xs1) (Var x2 xs2) 
  | x1 == x2, xs1 `isPrefixOf` xs2 = 
    return (drop (nlength xs1) xs2)
stripPrefix _ _ = Fail.here


-- | Returns zero if the given term takes no arguments.
argumentCount :: Term -> Nat
argumentCount (Var _ ts) = length ts
argumentCount (Def _ ts) = length ts
argumentCount (Con _ ts) = length ts
argumentCount _ = 0


stripArgs :: Fail.Can m => Nat -> Term -> m (Term, [Term])
stripArgs 0 t = return (t, [])
stripArgs n t
  | argumentCount t < n = Fail.here
stripArgs n (Var x ts) = return (Var x (drop n ts), take n ts)
stripArgs n (Def a ts) = return (Def a (drop n ts), take n ts)
stripArgs n (Con c ts) = return (Con c (drop n ts), take n ts)

  
-- | Given a type constructor this will return a fully instantiated 
-- constructor term as it would appear in a pattern match.
-- E.g. @altPattern ([list], 1) == Cons _1 _0@
constructorPattern :: Constructor -> Term
constructorPattern con@(Constructor (Ind _ _ cons) n) = id
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
    
    
-- | The arguments to a constructor term which have the same type
-- as the constructor itself.
recursiveConArgs :: Term -> [Term]
recursiveConArgs (Con con args) = 
  map (args !!) (Type.recursiveArgs con)
  
    
-- | Whether a term contains a finite amount of information, from a
-- strictness point of view. So @[x]@ is finite, even though @x@ is a variable
-- since it is not of the same type as the overall term.
isFinite :: Term -> Bool
isFinite (Con con args) = id
  . all isFinite 
  . map (args !!)
  $ Type.recursiveArgs con
isFinite _ = False


-- | A /simple/ term contains only contructors and variables.
isSimple :: Term -> Bool
isSimple (Con _ args) = all isSimple args
isSimple (Var _ args) = all isSimple args
isSimple _ = False


lowerableAltTerm :: Indexed Term => Alt -> Bool
lowerableAltTerm (Alt _ bs alt_t) =
  Indices.lowerableBy (length bs) alt_t

loweredAltTerm :: Indexed Term => Alt -> Term
loweredAltTerm (Alt _ bs alt_t) =
  Indices.lowerMany (length bs) alt_t
  
   
false, true :: Term
true = Con Type.true []
false = Con Type.false []

