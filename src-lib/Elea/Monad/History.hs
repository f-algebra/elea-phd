module Elea.Monad.History
(
  Repr, 
  Name,
  Env (..),
  EnvT,
  empty,
  size,
  embeds,
  insert,
  emptyEnvT,
  mapEnvT,
  check,
)
where

import Elea.Prelude hiding ( ask, local )
import Elea.Embed ( Code, Encodable )
import Elea.Transform.Names ( Name )
import qualified Elea.Embed as Embed
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Trans.Class as Trans 
import qualified Data.Poset as Partial
import qualified Data.IntMap as Map

data Repr 
  = Repr  { _reprSize :: !Nat
          , _reprMap :: !(IntMap [Code]) }
          
instance Show Repr where
  show = show . _reprMap
          
mkLabels [ ''Repr ]

embeds :: Name -> Code -> Repr -> Bool
embeds (fromEnum -> name) code repr = 
  case Map.lookup name (get reprMap repr) of
    Nothing -> False
    Just codes -> any (Partial.<= code) codes
    
insert :: Name -> Code -> Repr -> Repr
insert (fromEnum -> name) code = id
  . modify reprSize (+ 1)
  . modify reprMap (Map.alter add name)
  where
  add (Just codes) = Just (code:codes)
  add Nothing = Just [code]
  
empty :: Repr
empty = Repr 0 Map.empty

size :: Repr -> Nat
size = get reprSize

forgetName :: Name -> Repr -> Repr
forgetName k = modify reprMap (Map.delete (enum k))


class Monad m => Env m where
  ask :: m Repr
  local :: (Repr -> Repr) -> m a -> m a
  
  seeCode :: Name -> Code -> m a -> m a
  seeCode name code = local (insert name code)
  
  -- | Use our partial order on codes to check for embedding.
  -- The first argument is the step name that invoked this.
  seenCode :: Name -> Code -> m Bool
  seenCode name code = liftM (embeds name code) ask

  seen :: Encodable a => Name -> a -> m Bool
  seen name = seenCode name . Embed.encode
  
  see :: Encodable a => Name -> a -> m b -> m b
  see name = seeCode name . Embed.encode
  
  forget :: Name -> m a -> m a
  forget k = local (forgetName k)
  
  
check :: (Fail.Can m, Env m, Encodable a, Show a) => Name -> a -> m b -> m b
check name x continue = do
  fail <- seen name x
  if fail
  then id
   --  . trace ("\n\n[HISTORY BLOCKED <" ++ show name ++ ">] " ++ show x) 
     $ Fail.here
  else see name x continue
   

newtype EnvT m a
  = EnvT { envT :: ReaderT Repr m a }
  deriving ( Functor, Monad, MonadReader Repr, MonadTrans )

emptyEnvT :: EnvT m a -> m a
emptyEnvT = flip runReaderT empty  . envT

mapEnvT :: (m a -> n b) -> EnvT m a -> EnvT n b
mapEnvT f = EnvT . mapReaderT f . envT
  
instance Monad m => Env (EnvT m) where
  ask = Reader.ask
  local = Reader.local
    
instance Fail.Can m => Fail.Can (EnvT m) where
  here = Trans.lift Fail.here
  catch = mapEnvT Fail.catch
  
instance Env m => Env (MaybeT m) where
  ask = Trans.lift ask
  local f = mapMaybeT (local f)
  
instance Env m => Env (IdentityT m) where
  ask = Trans.lift ask
  local f = mapIdentityT (local f)
  

