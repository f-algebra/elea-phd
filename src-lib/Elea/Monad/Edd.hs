-- | The environment and definitions monad.
module Elea.Monad.Edd
(
  EddT, Edd,
  evalT, eval,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Show
import Elea.Monad.Fusion.Database ( Outcome (..) )
import qualified Elea.Context as Context
import qualified Elea.Monad.Discovery.EquationSet as EqSet
import qualified Elea.Monad.Definitions.Class as Defs
import qualified Elea.Monad.Definitions.Database as DefDB
import qualified Elea.Monad.Fusion.Class as Fusion
import qualified Elea.Monad.Fusion.Database as FusionDB
import qualified Elea.Monad.Parser.Class as Parser
import qualified Elea.Monad.Parser.Database as ParserDB
import qualified Elea.Monad.Env.Stack as EnvStack
import qualified Elea.Monad.Failure.Class as Fail
import qualified Elea.Monad.Env.Class as Env
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State.Class as State

newtype EddT m a 
  = EddT { 
    runEddT :: RWST EnvStack.Stack () EddState m a }
  deriving 
  ( Monad, MonadTrans
  , MonadReader EnvStack.Stack
  , MonadState EddState )
  
type Edd = EddT Identity

data EddState
  = ES  { _eddDefs :: !DefDB.Database
        , _eddParser :: !ParserDB.Database
        , _eddFusion :: !FusionDB.Database }
        
mkLabels [ ''EddState ]

emptyState :: EddState
emptyState = ES DefDB.empty ParserDB.empty FusionDB.empty

evalT :: Monad m => Edd m a -> m a
evalT = id
  . liftM fst3
  . (\rwst -> runRWST rwst EnvStack.empty emptyState)
  . runEddT
  where
  fst3 (x, _, _) = x
  
eval :: Edd a -> a
eval = runIdentity . evalT
  
instance Monad m => Parser.State (EddT m) where
  defineTerm n t = id
    . State.modify 
    . modify eddParser 
    $ ParserDB.defineTerm n t
    
  defineInd n i = id
    . State.modify 
    . modify eddParser 
    $ ParserDB.defineInd n i
    
    
instance Monad m => Defs.Read (EddT m) where
  get n = State.gets (DefDB.lookup n . get eddDefs)
  search _ = return Nothing
  
instance Monad m => Defs.Write (EddT m) where
  put = undefined
  
instance Monad m => Env.Write (EddT m) where
  bindAt at b = local (EnvStack.bindAt at b)
  matched t t' = local (EnvStack.matched t t')
  
instance Monad m => Env.Bindings (EddT m) where
  bindings = asks EnvStack.bindings
  
instance Monad m => Env.Matches (EddT m) where
  matches = asks EnvStack.matches
  
  
-- Memoise our fixpoint fusion calculations to improve speed
-- and stop potential loops.
instance Monad m => Fusion.Memo (EddT m) where
  memoiseMaybe ctx name name' = do
    fusion_db <- State.gets snd
    case FusionDB.lookup orig_t fusion_db of
      -- If we are in the process of fusing this then we need to fail
      -- to prevent infinite loops
      Just Pending -> id
        . trace ("[FusionDB] stopped a loop") 
        $ return Nothing
      
      -- If fusion failed previously then it will fail again
      Just Failure -> id
        . trace ("[FusionDB] improved efficiency (failure)") 
        $ return Nothing
      
      -- If fusion succeeded previously then we can be efficient 
      -- and return the previous result
      Just (Success t) -> id
        . trace ("[FusionDB] improved efficiency (success)") 
        $ return (Just t)
      
      -- If we have not yet tried this fusion then...
      Nothing -> do
        -- Set that we are performing this fusion
        let fusion_db' = FusionDB.insert orig_t Pending fusion_db
        State.modify (second (const fusion_db'))
        
        -- Run the fusion step
        mby_t <- run
        
        -- Store the result of this fusion
        let fusion_db'' =
              FusionDB.insert orig_t (FusionDB.maybeToOutcome mby_t) fusion_db
        State.modify (second (const fusion_db''))
        return mby_t
    where
    orig_t = Context.apply ctx term
    
