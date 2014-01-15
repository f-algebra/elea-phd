-- | The definitions monads provide the means to define terms and types
-- by name, and look them up later.
module Elea.Monad.Definitions
(
  Has, Read (..), Write (..), 
  Database, 
  emptyDatabase,
  DBReaderT, DBStateT, DBReader, DBState,
  readEmptyT, readEmpty,
  evalEmptyT, evalEmpty,
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import qualified Elea.Index as Indices
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

type Has m = (Read m, Write m)

class Monad m => Read m where
  lookupTerm :: String -> m (Maybe Term)
  lookupType :: String -> m (Maybe Type)
  
  -- | Given a term, attempt to find a name for this term, provided the name
  -- is given the list of terms as arguments.
  -- E.g. "termName (append xs ys) ==> ('append', [xs, ys])"
  lookupName :: Term -> m (Maybe (String, [Term]))
  
class Monad m => Write m where
  defineTerm :: String -> Term -> m ()
  defineType :: String -> Type -> m ()

instance Read m => Read (ReaderT r m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (ReaderT r m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (EitherT e m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (EitherT e m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
instance Read m => Read (MaybeT m) where
  lookupTerm = lift . lookupTerm
  lookupType = lift . lookupType
  lookupName = lift . lookupName
  
instance Write m => Write (MaybeT m) where
  defineTerm n = lift . defineTerm n
  defineType n = lift . defineType n
  
  
data Database
  = Database { _dbTerms :: Map String Term
             , _dbTypes :: Map String Type
             , _dbTermNames :: Map Term (Term, String) }

mkLabels [ ''Database ]

emptyDatabase :: Database
emptyDatabase = Database mempty mempty mempty
             
dbAddType :: String -> Type -> Database -> Database
dbAddType name ty =
  modify dbTypes (Map.insert name ty)
  
dbGetType :: Fail.Can m => String -> Database -> m Type
dbGetType name =
  Fail.mapLookup name . get dbTypes
  
dbGetTerm :: Fail.Can m => String -> Database -> m Term
dbGetTerm name =
  Fail.mapLookup name . get dbTerms
             
dbAddTerm :: String -> Term -> Database -> Database
dbAddTerm name term = id
  . modify dbTerms (Map.insert name term)
  . modify dbTermNames addTermName
  where
  addTermName :: Map Term (Term, String) -> Map Term (Term, String)
  addTermName
    -- We only enable name lookup for terms containing no free variables
    | (not . Set.null . Indices.free) term = id
    | otherwise = Map.insert omega_term (inner_term, name)
    where
    (_, inner_term) = flattenLam term
    
    -- We replace all free variables with omega, so that we can use 
    -- the logarithmic time map lookup to match terms
    omega_term = freeIndicesToOmega inner_term
        
    
dbTermName :: Fail.Can m => Term -> Database -> m (String, [Term])
dbTermName (Lam {}) _ = Fail.here
dbTermName term (get dbTermNames -> name_map) = do
  (inner_term, name) <- Fail.mapLookup omega_term name_map
  uni <- Unifier.find term inner_term

  -- Every free variable needs to have been bound, so we just check the 
  -- sorted list of indices is equal to the list of all indices
  let uni_list = sort (Map.toList uni)
  Fail.unless (map (enum . fst) uni_list == [0..length uni_list - 1])
  
  let arg_list = reverse (map snd uni_list)
  return (name, arg_list)
  where
  omega_term = freeIndicesToOmega term
  
  
-- | Replace all free indices in a term with omega.
freeIndicesToOmega :: Term -> Term
freeIndicesToOmega = id
  . Env.trackOffset
  . Fold.transformM omegafy 
  where
  omegafy :: Term -> Env.TrackOffset Term
  omegafy (Var x) = do
    offset <- Env.offset
    if enum x >= offset 
    then return (Var Indices.omega)
    else return (Var x)
  omegafy other = 
    return other
  
      
newtype DBReaderT m a 
  = DBReaderT { runDBReaderT :: ReaderT Database m a }
  deriving ( Monad, MonadTrans, MonadReader Database )
  
type DBReader = DBReaderT Identity
  
newtype DBStateT m a 
  = DBStateT { runDBStateT :: StateT Database m a }
  deriving ( Monad, MonadTrans, MonadState Database )
  
type DBState = DBStateT Identity
  
readEmptyT :: Monad m => DBReaderT m a -> m a
readEmptyT = flip runReaderT emptyDatabase . runDBReaderT

readEmpty :: DBReader a -> a
readEmpty = runIdentity . readEmptyT

evalEmptyT :: Monad m => DBStateT m a -> m a
evalEmptyT = flip evalStateT emptyDatabase . runDBStateT

evalEmpty :: DBState a -> a 
evalEmpty = runIdentity . evalEmptyT
    
instance Monad m => Read (DBReaderT m) where
  lookupTerm = asks . dbGetTerm
  lookupType = asks . dbGetType
  lookupName = asks . dbTermName
  
instance Monad m => Read (DBStateT m) where
  lookupTerm = State.gets . dbGetTerm
  lookupType = State.gets . dbGetType
  lookupName = State.gets . dbTermName
  
instance Monad m => Write (DBStateT m) where
  defineTerm n = State.modify . dbAddTerm n
  defineType n = State.modify . dbAddType n

