-- | Some term transformation steps that rely on fixpoint fission.
module Elea.Fission
(
  steps, run
)
where

import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import Elea.Monad.Fedd ( Fedd )
import qualified Elea.Fixpoint as Fix
import qualified Elea.Index as Indices
import qualified Elea.Monad.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Context as Context
import qualified Elea.Unification as Unifier
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Error.Class as Err
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Definitions as Defs
import qualified Elea.Foldable as Fold
import qualified Data.Set as Set

{-# SPECIALISE run :: Term -> Fedd Term #-}

run :: (Defs.Read m, Env.Read m) => Term -> m Term
run = Fold.rewriteStepsM (map Type.checkStep steps)

steps :: (Env.Read m, Fail.Can m, Defs.Read m) => [Term -> m Term]
steps = Simp.steps ++ 
  [ const Fail.here
  , identityFix
  , constructorFission
  ]                   


-- | Remove a fixpoint which recursively returns one of its arguments
identityFix :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) => 
  Term -> m Term
identityFix fix@(Fix _ (Bind _ fix_ty) fix_t) = do
  -- Just in case we have a function where not all lambdas are topmost,
  -- even though this shouldn't occur
  Fail.unless (length arg_bs == (length arg_tys :: Int))
  
  Fail.choose (map identityOn potential_args)
  where
  arg_tys = Type.argumentTypes fix_ty
  res_ty = Type.returnType fix_ty
  
  -- Potential argument indices are those whose type matches the return type
  -- of the function
  potential_args = findIndices (== res_ty) arg_tys
  
  -- Pull the bindings for the arguments
  (arg_bs, _) = Term.flattenLam fix_t
  
  -- See if the function is an identity on the given argument index
  identityOn :: Int -> m Term
  identityOn arg_i = 
    Fix.fission (return . Simp.run) fix ctx
    where
    -- A constant context which returns the identity function
    -- on the given argument index
    lam_var = (length arg_tys - arg_i) - 1
    ctx = Context.make (\_ -> unflattenLam arg_bs (Var (enum lam_var)))
  
identityFix _ = Fail.here


-- | Attempts to simplify fixpoints to head normal form. Floats a constructor
-- which the fixpoint will always return outside the fixpoint.
constructorFission :: forall m . (Env.Read m, Fail.Can m, Defs.Read m) =>
  Term -> m Term
  
constructorFission fix@(Fix _ fix_b fix_t) = do
  Fail.when (Set.null suggestions)
  Fail.unless floatable
  
  -- Pick the first successful fission step
  Fail.choose
    -- Attempt fission on each of the suggested constructors
    . map (Fix.fission (return . Simp.run) fix)
    $ toList suggestions
  where 
  return_ty = Type.returnType (get Type.boundType fix_b)
  (arg_bs, _) = Term.flattenLam fix_t
  absurd_ctx = id
    . Context.make 
    . const
    . unflattenLam arg_bs
    $ Unr return_ty
    
  -- A very quick check as to whether we can float a constructor out of
  -- this fixpoint. Makes sure that only one constructor is returned down
  -- every branch.
  floatable :: Bool
  floatable = cons == 1
  --  || (cons == 0 && suggestions == Set.singleton absurd_ctx)
    where
    cons = Set.size (returnCons fix_t)
    
    -- Return the index of every constructor returned
    -- by this term.
    returnCons :: Term -> Set Nat
    returnCons (leftmost -> Con _ n) = Set.singleton n
    returnCons (Lam _ t) = returnCons t
    returnCons (Case _ _ alts) = 
      Set.unions (map (returnCons . get altInner) alts)
    returnCons _ = mempty

  -- The set of constructor contexts we should try fission on
  suggestions :: Set Context
  suggestions = id
    . fromMaybe mempty
    . Env.trackOffset
    . Env.liftTracked
    . runMaybeT
    $ suggest fix_t
    where
    suggest :: Term -> MaybeT Env.TrackOffset (Set Context)
    suggest (Lam _ t) = 
      Env.liftTracked (suggest t)
    suggest (Case ind _ alts) =
      concatMapM suggestAlt alts
      where
      suggestAlt :: Alt -> MaybeT Env.TrackOffset (Set Context)
      suggestAlt (Alt bs alt_t) =
        Env.liftTrackedMany (length bs) (suggest alt_t)
        
    suggest con_t@(Term.flattenApp -> Con ind con_n : args) = do
      free_limit <- Env.tracked
      -- We cannot keep an argument to the constructor if it contains
      -- variables which are not free outside the fixpoint,
      -- because we cannot float these variables out.
      let not_keepable = 
            findIndices (any (< free_limit) . Indices.free) args
            
      -- If we have more than one not keepable argument then none
      -- of them can be the gap, so we fail.
      Fail.when (length not_keepable > 1)
      
      let idx_offset = enum (free_limit - length arg_bs)
      
      if length not_keepable == 1
      then do
        -- If only one argument is not keepable, then we have to
        -- use this argument for the gap
        let arg_i = head not_keepable
        -- But this cannot be the gap if it's not recursive
        Fail.when (arg_i `elem` Type.nonRecursiveArgs ind con_n)
        return
          . Set.singleton
          $ gapContext idx_offset arg_i
      else return
         . Set.unions
         . map (Set.singleton . gapContext idx_offset) 
         $ Type.recursiveArgs ind con_n
      where
      -- Construct a context which is the constructor term with
      -- a gap at the given argument position
      gapContext :: Nat -> Int -> Context
      gapContext idx_offset gap_i =
        Context.make mkContext
        where
        (left, _:right) = splitAt gap_i args
                
        mkContext gap_f = id
          -- Since the gap will be a function, we need to apply arguments
          -- to it, so we need to abstract these arguments first.
          -- For example, when we float "Cons x _" to the top of 
          -- "reverse (append xs (Cons x Nil))", our context is
          -- "fun (ys: nlist) -> Cons x (_ ys)".
          . unflattenLam arg_bs
          . app con'
          $ left' ++ [gap_t] ++ right'
          where
          con' = Indices.lowerMany idx_offset (Con ind con_n)
          left' = map (Indices.lowerMany idx_offset) left
          right' = map (Indices.lowerMany idx_offset) right
          -- Apply the arguments we lambda'd at the top of the context
          -- to the function that will be supplied at the gap.
          gap_t = id
            . app gap_f
            . map (Var . toEnum) 
            $ reverse [0..length arg_bs - 1]
            
    suggest term = do
      fix_f <- liftM pred Env.offset
      -- Fail if we've reached a return value, which is not in HNF,
      -- and also doesn't contain the recursive function call.
      Fail.unless (enum fix_f `Indices.freeWithin` term)
      return mempty
      
constructorFission _ = 
  Fail.here
  
