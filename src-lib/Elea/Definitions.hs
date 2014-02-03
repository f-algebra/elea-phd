-- | A concrete implementation of a database of term definitions (let bindings),
-- along with a reader and state monad just for this database type.
module Elea.Definitions
(
  module Elea.Monad.Definitions,

  Database, 
  emptyDatabase,
 
  DBReaderT, DBReader,
  readEmptyT, readEmpty,
  
  DBStateT, DBState,
  evalEmptyT, evalEmpty
)
where

import Prelude ()
import Elea.Prelude hiding ( Read )
import Elea.Term
import Elea.Monad.Definitions
import Elea.Unifier ( Unifier )
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Monad.Failure as Fail
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
  
data Database
  = Database { _dbTerms :: !(Map String Term)
             , _dbTypes :: !(Map String Type)
             , _dbTermNames :: ![(Term, String)] }

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
             
dbAddTerm :: Show Term => String -> Term -> Database -> Database
dbAddTerm name term
  | (not . Set.null . Indices.free) term =
    error "Cannot define term containing free variables."
    
  | otherwise = id
    . modify dbTerms (Map.insert name term)
    . modify dbTermNames addTermName
  where
  addTermName
    | (isFix . leftmost) inner_term = (++ [(inner_term, name)]) 
    | otherwise = id
  
  (_, inner_term) = flattenLam term
        
    
dbTermName :: (Show Term, Fail.Can m) => Term -> Database -> m (String, [Term])
dbTermName (Lam {}) _ = Fail.here
dbTermName term _ 
  | (not . isFix . leftmost) term = Fail.here
dbTermName term (get dbTermNames -> name_list) = do
  (uni, name) <- Fail.choose (map unifiable name_list)
  
  -- Every free variable needs to have been bound, so we just check the 
  -- sorted list of indices is equal to the list of all indices
  let uni_list = sort (Map.toList uni)
  Fail.unless (map (enum . fst) uni_list == [0..length uni_list - 1])
  
  let arg_list = id
        . map (Indices.lowerMany large_number)
        . reverse 
        $ map snd uni_list
        
  return (name, arg_list)
  where
  -- This is a hack to get identity variable unifications to show up, 
  -- e.g. if we need to rewrite index 2 to index 2 we want this to show up
  -- in the unification, so we just add 100000 to every index so they
  -- won't clash
  large_number = 100000
  term' = Indices.liftMany large_number term
  
  unifiable :: Fail.Can m => (Term, String) -> m (Unifier Term, String)
  unifiable (inner_term, name) = do
    uni <- Unifier.find inner_term term'
    return (uni, name)
  
      
newtype DBReaderT m a 
  = DBReaderT { runDBReaderT :: ReaderT Database m a }
  deriving ( Monad, MonadTrans, MonadReader Database )
  
type DBReader = DBReaderT Identity

readEmptyT :: Monad m => DBReaderT m a -> m a
readEmptyT = flip runReaderT emptyDatabase . runDBReaderT

readEmpty :: DBReader a -> a
readEmpty = runIdentity . readEmptyT

instance (Show Term, Monad m) => Read (DBReaderT m) where
  lookupTerm = asks . dbGetTerm
  lookupType = asks . dbGetType
  lookupName = asks . dbTermName
  
  
newtype DBStateT m a 
  = DBStateT { runDBStateT :: StateT Database m a }
  deriving ( Monad, MonadTrans, MonadState Database )
  
type DBState = DBStateT Identity
  
evalEmptyT :: Monad m => DBStateT m a -> m a
evalEmptyT = flip evalStateT emptyDatabase . runDBStateT

evalEmpty :: DBState a -> a 
evalEmpty = runIdentity . evalEmptyT

mapDBStateT :: (m (a, Database) -> n (b, Database)) -> DBStateT m a -> DBStateT n b
mapDBStateT f = DBStateT . mapStateT f . runDBStateT

instance (Show Term, Monad m) => Read (DBStateT m) where
  lookupTerm = State.gets . dbGetTerm
  lookupType = State.gets . dbGetType
  lookupName = State.gets . dbTermName
  
instance (Show Term, Monad m) => Write (DBStateT m) where
  defineTerm n = State.modify . dbAddTerm n
  defineType n = State.modify . dbAddType n
  
instance Env.Write m => Env.Write (DBStateT m) where
  bindAt at b = mapDBStateT (Env.bindAt at b)
  matched t w = mapDBStateT (Env.matched t w)
  
instance Env.Read m => Env.Read (DBStateT m) where
  bindings = Trans.lift Env.bindings
