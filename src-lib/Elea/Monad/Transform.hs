-- | A transformation monad carries around a 'Term -> m Term' function
-- allowing it to apply the transformation recursively.
module Elea.Monad.Transform
(
  Rule (..),
  RuleT (..),
  Step,
  runRule,
  runStep,
  traceCont,
  mapRuleT,
)
where

import Elea.Prelude
import Elea.Term 
import Elea.Show ()
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans


-- | Accessing the recursive call to our transformation 
class Monad m => Rule m where
  continue :: Term -> m Term
  
type Step m = (Rule m, Fail.Can m, Env.Write m)
  
-- | Carry around a call to a simplification function
newtype RuleT m a 
  = RuleT { ruleT :: ReaderT (Term -> m Term) m a }
  deriving ( Functor, Monad )
  
mapRuleT :: Monad m => (m a -> m b) -> RuleT m a -> RuleT m b
mapRuleT f = RuleT . mapReaderT f . ruleT

-- | Feeds a function in as its own recursive call
runRule :: Monad m => (Term -> RuleT m Term) -> Term -> m Term
runRule f t = runReaderT (ruleT (f t)) (runRule f)

runStep :: Monad m => (Term -> MaybeT (RuleT m) Term) -> Term -> m Term
runStep f = runRule (\t -> Fail.withDefault t (f t))


traceCont :: Rule m => String -> Term -> Term -> m Term
traceCont step_name orig new = 
  trace 
    ("\n\n[" ++ step_name ++ "]\n" ++ show orig ++ "\n==>\n" ++ show new) 
    (continue new)


instance MonadTrans RuleT where
  lift m = RuleT (ReaderT (\_ -> m))
  
instance Monad m => Rule (RuleT m) where
  continue t = do
    f <- RuleT Reader.ask
    Trans.lift (f t)
    
instance Fail.Can m => Fail.Can (RuleT m) where
  here = Trans.lift Fail.here
  catch = mapRuleT Fail.catch
    
instance Rule m => Rule (MaybeT m) where
  continue = Trans.lift . continue
  
instance Env.Read m => Env.Read (RuleT m) where
  bindings = Trans.lift Env.bindings
  
instance Env.Write m => Env.Write (RuleT m) where
  bindAt at b = mapRuleT (Env.bindAt at b)
  matched t c = mapRuleT (Env.matched t c)

instance Defs.Read m => Defs.Read (RuleT m) where
  lookupTerm n = Trans.lift . Defs.lookupTerm n
  lookupType n = Trans.lift . Defs.lookupType n
  lookupName = Trans.lift . Defs.lookupName
  

