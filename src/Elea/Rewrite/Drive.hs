module Elea.Rewrite.Drive (
  rewrite, inc
) where

import Elea.Prelude
import Elea.Term
import Elea.Monad.Direction ( Direction )
import qualified Elea.Type as Type
import qualified Elea.Term.Tag as Tag
import qualified Elea.Term.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Term.Ext as Term
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Direction as Direction
import qualified Elea.Monad.Error.Assertion as Assert

-- TODO try making function application/composition strict by overriding (.) and ($)
-- or use bang patterns on arguments of apply
-- or GHC 8 and strict pragma?

-- TODO substMany for beta and case reduction

-- TODO closed fixed-points don't need to be descended into
-- on a similar note, precompute strict/decreasing/increasing argument indices

inc :: Term -> Term
inc = id
  . flip runReader Direction.Inc
  . runDrive
  . apply

rewrite :: Direction.Has m => Term -> m Term
rewrite term = do
  direction <- Direction.get
  return 
    . flip runReader direction
    . runDrive 
    $ apply term

newtype Drive a = Drive { 
  runDrive :: Reader Direction a
} deriving ( Functor, Applicative, Monad, MonadReader Direction )


instance Env.Write Drive where
  bindAt _ _ = id
  matched _ = id
  forgetMatches _ = id

instance Direction.Has Drive where
  local new_dir = local (const new_dir)
  get = ask


type Env m = (Env.Write m, Direction.Has m)

-- | Apply driving, the monad will eventually carry information 
-- about strictness, totality, and termination checking
apply :: forall m . Env m => Term -> m Term 
apply term = do
  term' <- (applyHead <=< applySubterms) term
  Assert.checkM $ Term.assertValidRewrite term term' 
  return term'
  where
  applySubterms :: Term -> m Term
  applySubterms (Lam b t) = do
    t' <- Env.bind b (apply t)
    return $ Lam b t'
  applySubterms fix@Fix { binding = b, inner = t } = do
    t' <- Env.bind b (apply t)
    return $ fix { inner = t' }
  applySubterms (Leq left right) = do
    left' <- apply left
    right' <- Direction.invert (apply right)
    return $ Leq left' right'
  applySubterms (Seq seq_of inner) = do
    seq_of' <- apply seq_of
    inner' <- apply inner
    return $ Seq seq_of' inner'
  applySubterms (App f xs) = do
    f' <- apply f
    xs' <- mapM apply xs
    return $ App f' xs'
  applySubterms cse_of@(Case cse_t alts) = do
    cse_t' <- apply cse_t
    alts' <- zipWithM applyAlt [0..] alts
    return $ Case cse_t' alts'
    where
    applyAlt branch_n alt@Alt { _altInner = alt_t } = do
      alt_t' <- id
        . Env.bindBranch cse_of branch_n
        . apply
        $ alt_t
      return $ alt { _altInner = alt_t' }

  applySubterms other =
    return other


-- | Drive a term for which we know all sub-terms are already driven
applyHead :: forall m . Env m => Term -> m Term

-- normalise
applyHead (App f []) = 
  return f
applyHead (App (App f xs) xs') =
  return $ App f (xs ++ xs')

-- strictness
applyHead term
  | isUndef term = 
    return $ Bot (Type.get term)
  where 
  -- Needs a "strict argument is bot checker"
  isUndef (App (Bot _) _) = True
  isUndef (Case (Bot _) _) = True
  isUndef (Case _ alts)
    | all (isBot . get altInner) alts = True
  isUndef _ = False

-- beta reduction
applyHead (App f@Lam{} xs)
  | all isVar xs = return reduced
  | otherwise = apply reduced
  where
  reduced = Term.reduce f xs

-- case-of constructor reduction
applyHead (Case (flattenApp -> Con tcon : args) alts)
  | all isVar args = return branch_t
  | otherwise = apply branch_t
  where
  Alt { _altBindings = binds, _altInner = alt_t } = id
    . (alts !!)
    . get Type.constructorIndex
    $ Tag.untag tcon

  branch_t = id
    -- We fold substitute over the arguments to the constructor
    -- starting with the return value of the pattern match (alt_t).
    -- So we substitute each argument in one by one to the alt term.
    . foldr Indices.subst alt_t
    -- When we substitute an constructor argument, 
    -- it needs to not be affected by the substitution of later arguments.
    -- So we lift their indices a number of times
    -- depending on their position in the order of substitution, 
    -- viz. those substituted first are lifted the most.
    $ zipWith Indices.liftMany [0..] args

-- case-of as a function
applyHead (App cse_of@(Case cse_t alts) args) = do
  alts' <- zipWithM applyAlt [0..] alts
  applyHead $ Case cse_t alts'
  where
  applyAlt branch_n alt@Alt { _altBindings = binds, _altInner = alt_t } = do
    alt_t' <- id
      . Env.bindBranch cse_of branch_n
      . applyHead 
      $ App alt_t args'
    return $ alt { _altInner = alt_t' }
    where
    args' = Indices.liftMany (nlength binds) args

-- unfold a fixed-point if any decreasing arguments are finite (no variables)
applyHead term@(App fix@Fix {} args) 
  | any (Term.isFinite . (args !!)) dec_is =
    apply $ Term.reduce (Term.unfoldFix fix) args
  where
  dec_is = Term.decreasingAppArgs term

-- case-of as a strict argument
applyHead (App func@Fix {} args) 
  | isJust mby_cse = do
    alts' <- zipWithM applyAlt [0..] alts
    applyHead $ Case cse_t alts'
  where
  mby_cse@(~(Just cse_i)) = findIndex isCase args
  cse_of@(Case cse_t alts) = args !! cse_i 

  applyAlt branch_n alt@Alt { _altInner = alt_t, _altBindings = binds } = do
    alt_t' <- id
      . Env.bindBranch cse_of branch_n
      . applyHead 
      $ App func' args'
    return $ alt { _altInner = alt_t' }
    where
    func' = Indices.liftMany (nlength binds) func
    args' = id 
      . setAt cse_i alt_t
      $ Indices.liftMany (nlength binds) args

-- case-case distributivity
applyHead outer_cse@(Case inner_cse@(Case inner_t inner_alts) outer_alts) = do
  inner_alts' <- zipWithM applyAlt [0..] inner_alts
  applyHead $ Case inner_t inner_alts'
  where
  applyAlt branch_n alt@Alt { _altBindings = binds, _altInner = alt_t } = do
    alt_t' <- id
      . Env.bindBranch inner_cse branch_n
      . applyHead 
      $ Case alt_t outer_alts'
    return $ alt { _altInner = alt_t' }
    where
    outer_alts' = map (Indices.liftMany (nlength binds)) outer_alts 

-- identity case removal
applyHead (Case cse_t alts) 
  | all isIdAlt alts = return cse_t
  where
  isIdAlt :: Alt -> Bool
  isIdAlt alt@Alt { _altInner = alt_t } =
    alt_t == (patternTerm . altPattern) alt

-- applying pattern matches over variables as a rewrite
applyHead term@(Case cse_t@Var { varIndex = var } alts) 
  | any (Indices.freeWithin var) alts = do
    alts' <- zipWithM applyAlt [0..] alts
    applyHead $ Case cse_t alts'
    where
    applyAlt branch_n alt@Alt { _altBindings = binds, _altInner = alt_t } = do
      alt_t' <- id
        . Env.bindBranch term branch_n
        . apply 
        . replaceVarWithPattern
        $ alt_t
      return $ alt { _altInner = alt_t' }
      where
      pat_t = (patternTerm . altPattern) alt
      replaceVarWithPattern = 
        Indices.replaceAt (Indices.liftMany (nlength binds) var) pat_t


-- theorem prover steps start here

-- forall A . _|_ =< A
applyHead (Leq (Bot _) _) = return truth

-- (tt ==> P) == P
applyHead (Leq p tt) 
  | tt == truth = return p

-- (ff ==> P) == tt
applyHead (Leq _ ff)
  | ff == falsity = return truth

-- double negation elimination
applyHead (Leq ff1 (Leq ff2 t)) 
  | ff1 == falsity
  , ff2 == falsity = 
    return t

-- constructor injectivity
applyHead (Leq (flattenApp -> Con tc : xs) (flattenApp -> Con tc' : xs'))
  | Tag.untag tc /= Tag.untag tc' = return falsity
  | otherwise = do
    apply
      . conj
      $ zipWith Leq xs xs'

-- floating lambdas out of the left and right hand side of (=<)
applyHead leq@(Leq (Lam bind left) right) = do
  right' <- applyHead (App (Indices.lift right) [Var 0 bind])
  leq' <- applyHead (Leq left right')
  applyHead $ Lam bind leq'
applyHead leq@(Leq left (Lam bind right)) = do
  left' <- applyHead (App (Indices.lift left) [Var 0 bind])
  leq' <- applyHead (Leq left' right)
  applyHead $ Lam bind leq'

-- (forall x . tt) == tt, (forall x . ff) == ff
applyHead (Lam _ t)
  | t == Term.truth || t == Term.falsity = 
    return t

-- reflexivity of =< 
applyHead (Leq x y) 
  | x == y = return truth

-- otherwise, apply the steps which require a specific rewrite direction
applyHead term = do
  direction <- Direction.get
  case direction of
    Direction.Inc -> applyHeadInc term
    Direction.Dec -> applyHeadDec term
    _ -> return term


-- | Drive a term, assuming all subterms are already driven, 
-- and we are increasing definedness
applyHeadInc :: Env m => Term -> m Term

-- least fixed-point rule
applyHeadInc t@(Leq fix right) 
  | isFix (leftmost fix)
  , isFixPromoted fix = do
    fix' <- id
      . apply 
      . (\f -> Term.reduce f args)
      . Indices.subst (Term.abstractVars args right) 
      $ fix_t
    applyHead $ Leq fix' right
  where
  Fix { inner = fix_t } : args = flattenApp fix

applyHeadInc leq@(Leq (Case cse_t alts) right) = do
  alts' <- mapM leqAlt alts
  applyHead $ Case cse_t alts'
  where
  leqAlt alt@Alt { _altBindings = binds, _altInner = alt_t } = do
    alt_t' <- applyHead $ Leq alt_t right'
    return $ alt { _altInner = alt_t' }
    where
    right' = Indices.liftMany (nlength binds) right

applyHeadInc other = 
  return other


-- | Drive a term, assuming all subterms are already driven, 
-- and we are decreasing definedness
applyHeadDec :: Env m => Term -> m Term
applyHeadDec leq@(Leq left (Case cse_t alts)) = do
  alts' <- mapM leqAlt alts
  applyHead $ Case cse_t alts'
  where
  leqAlt alt@Alt { _altBindings = binds, _altInner = alt_t } = do
    alt_t' <- applyHead $ Leq left' alt_t
    return $ alt { _altInner = alt_t' }
    where
    left' = Indices.liftMany (nlength binds) left

applyHeadDec other = 
  return other
