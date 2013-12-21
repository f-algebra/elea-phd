-- | Some term transformation steps that rely on fixpoint fission.
module Elea.Fission
(
  steps, run
)
where

import Prelude ()
import Elea.Prelude
import Elea.Term
import Elea.Context ( Context )
import Elea.Show ( showM )
import qualified Elea.Fixpoint as Fix
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Terms as Term
import qualified Elea.Types as Type
import qualified Elea.Context as Context
import qualified Elea.Unifier as Unifier
import qualified Elea.Simplifier as Simp
import qualified Elea.Monad.Error as Err
import qualified Elea.Monad.Failure as Fail
import qualified Elea.Foldable as Fold

run :: Env.Readable m => Term -> m Term
run = Fold.rewriteStepsM (map Type.checkStep steps)

steps :: (Env.Readable m, Fail.Can m) => [Term -> m Term]
steps = Simp.steps ++ 
  [ const Fail.here
  , identityFix
  ]                   


-- | Remove a fixpoint which recursively returns the original argument
identityFix :: (Env.Readable m, Fail.Can m) => Term -> m Term
identityFix fix@(Fix (Bind _ (Type.Fun arg_ty res_ty)) _)
  -- We can quickly identify potential functions as 
  -- they will have type @A -> A@ where @A@ is inductive. 
  | Type.isInd arg_ty
  , arg_ty == res_ty = 
    Fix.fission simplify fix ctx
  where
  -- A constant context which returns the identity function
  ctx = Context.make (\_ -> Lam (Bind "x" arg_ty) (Var 0))
  
  -- The sub-simplification is just the regular simplifier
  simplify _ _ = Simp.run
  
identityFix _ = Fail.here
