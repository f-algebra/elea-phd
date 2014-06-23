-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  module Elea.Term,
  branches,
  replaceName,
  replace,
  applyCase,
)
where

import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Term as Term
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions.Class as Defs
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
      

-- | Replace all instances of a function name with the given variable index.
replaceName :: Inst Name -> Index -> Term -> Term
replaceName iname var = 
  Env.trackOffset . Fold.transformM replace
  where
  replace :: Term -> Env.TrackOffset Term
  replace (Def ty_name args) 
    | fmap typedObj ty_name == iname = do
      var' <- Env.liftByOffset var
      return (Var var' args)
  replace term =
    return term
    
    
-- | Replace all instances of the first term with the second within the third.
replace :: Term -> Term -> Term -> Term
replace me with = id
  . Env.trackIndices (me, with)
  . Fold.transformM doReplace
  where
  -- 'Env.TrackIndices' is needed to make sure indices
  -- are properly updated as we move inside 
  -- the term, e.g. if we pass inside case-of bindings.
  doReplace :: Term -> Env.TrackIndices (Term, Term) Term
  doReplace term = do
    (me, with) <- Env.tracked
    case Term.stripPrefix me term of
      Nothing -> return term
      Just args -> return (apply with args)
    
    
-- | Take a case-of term and replace the result term down each branch
-- with the second term argument.
applyCase :: Term -> Term -> Term
applyCase (Case cse_t alts) inner_t = 
  Case cse_t (map mkAlt alts)
  where
  mkAlt :: Alt -> Alt
  mkAlt (Alt con bs _) = 
    Alt con bs alt_t
    where
    -- Takes a term from outside the pattern match and lifts the 
    -- indices to what they should be within this branch
    liftHere = Indices.liftMany (length bs)
    
    -- The new alt-term is the given inner_t, with all occurrences of
    -- the pattern matched term replaced with the pattern it is matched
    -- to down this branch.
    pat = Term.constructorPattern con
    alt_t = replace (liftHere cse_t) pat (liftHere inner_t)
    

