-- | Here I've put all the helper functions dealing with 
-- 'Term's, but which also require other modules based on Elea.Term.
module Elea.Terms
(
  module Elea.Term,
  replace, 
  unfoldFix,
  decreasingArgs,
  applyCase,
  generaliseArgs,
  pair,
  buildFold,
  constraint,
)
where

import Prelude ()
import Elea.Prelude hiding ( replace )
import Elea.Term
import Elea.Context ( Context )
import qualified Elea.Types as Type
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Context as Context
import qualified Elea.Evaluation as Eval
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Control.Monad.Trans as Trans

unfoldFix :: Term -> Term
unfoldFix fix@(Fix _ _ fix_t) = 
  Indices.subst fix fix_t
  

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
    fix_f = enum (length arg_bs)
    
    decreasing :: 
      Term -> Env.TrackSmallerTermsT (Env.TrackIndices Index) Bool
    decreasing t@(App (Var f) args) = do
      fix_f <- Trans.lift Env.tracked
      if fix_f /= f
      then return True
      else Env.isSmaller (args !! arg_i)
    decreasing _ = 
      return True
      
      
-- | Take a case-of term and replace the result term down each branch
-- with the second term argument.
applyCase :: Term -> Term -> Term
applyCase (Case ind cse_t alts) inner_t = 
  Case ind cse_t (zipWith mkAlt [0..] alts)
  where
  mkAlt :: Int -> Alt -> Alt
  mkAlt n (Alt bs _) = Alt bs alt_t
    where
    -- Takes a term from outside the pattern match and lifts the 
    -- indices to what they should be within this branch
    liftHere = Indices.liftMany (nlength bs)
    
    -- The new alt-term is the given inner_t, with all occurrences of
    -- the pattern matched term replaced with the pattern it is matched
    -- to down this branch.
    pat = altPattern ind (enum n)
    alt_t = replace (liftHere cse_t) pat (liftHere inner_t)

 
-- | Generalise all the arguments of a term to fresh variables.
-- The first argument of the inner computation to run will lift 
-- indices by the number of new variables.
generaliseArgs :: (Indexed a, Substitutable t, 
                   Inner t ~ Term, Env.Read m, Defs.Read m) =>
  Term -> ((a -> a) -> Term -> m t) -> m t
generaliseArgs (App func args) run = do
  -- Use the type of every arguments to generate bindings for our new 
  -- generalised variables.
  arg_tys <- mapM Type.get args
  let gen_bs = zipWith (\n -> Bind ("X" ++ show n)) [0..] arg_tys
        
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
  new_vars = map Var [0..elength args - 1]
  
  liftHere :: Indexed b => b -> b
  liftHere = Indices.liftMany (elength args)
        
  
-- | Construct a pair of the two given terms. Needs to read the type of the
-- two terms so it can construct the appropriate cartesian product type for
-- the constructor.
pair :: (Defs.Read m, Env.Read m) => Term -> Term -> m Term
pair left right = do
  left_ty <- Type.get left
  right_ty <- Type.get right
  let pair_ind = Type.pair left_ty right_ty
  return (app (Con pair_ind 0) [left, right])
  
  
-- | Build a term representing a catamorphism (fold function).
buildFold :: () 
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
    
      
-- | Create a constraint context. For example:
-- > constraint (reverse xs) 0 nat == assert Nil <- reverse xs in _
-- > constraint (reverse xs) 1 nat == assert Cons y ys <- reverse xs in _
constraint :: (Env.Read m, Defs.Read m)
  => Term -- ^ The term we constrain to be a specific constructor
  -> Nat  -- ^ The constructor index we are constraining this term to be
  -> Type -- ^ The type of the gap in the context
  -> m Context 
constraint match_t con_n result_ty = do
  Type.Base ind <- Type.get match_t
  return (Context.make (makeContext ind))
  where
  makeContext ind gap_t = 
    Case ind match_t alts
    where
    cons = Type.unfold ind
    alts = map buildAlt [0..length cons - 1]
    
    buildAlt :: Int -> Alt
    buildAlt alt_n = 
      Alt bs alt_t
      where
      Bind _ con_ty = cons !! alt_n
      bs = map (Bind "X") (init (Type.flatten con_ty))
      
      -- If we are not down the matched branch we return absurd
      alt_t | enum alt_n /= con_n = Absurd result_ty
            | otherwise = gap_t
            
