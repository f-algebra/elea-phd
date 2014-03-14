-- | A concrete implementation of a database of term definitions (let bindings),
-- along with a reader and state monad just for this database type.
module Elea.Monad.Definitions
(
  module Elea.Monad.Definitions.Class,

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
import Elea.Type ( Polymorphic, Ind (..) )
import Elea.Monad.Definitions.Class
import Elea.Unifier ( Unifier )
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Env as Env
import qualified Elea.Unifier as Unifier
import qualified Elea.Monad.Discovery.Class as Discovery
import qualified Elea.Monad.Failure.Class as Fail
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
  
data Database
  = Database { _dbTerms :: !(Map String (Polymorphic Term))
             , _dbTypes :: !(Map String (Polymorphic Ind))
             , _dbTermNames :: ![(Term, String)] }

mkLabels [ ''Database ]

emptyDatabase :: Database
emptyDatabase = Database mempty mempty mempty
             
dbAddType :: String -> Polymorphic Ind -> Database -> Database
dbAddType name ty =
  modify dbTypes (Map.insert name ty)
  
dbGetType :: Fail.Can m => String -> [Type] -> Database -> m Ind
dbGetType name ty_args db = do
  p_ind <- Fail.mapLookup name (get dbTypes db)
  return (Type.monomorphic ty_args p_ind)
             
dbAddTerm :: String -> Polymorphic Term -> Database -> Database
dbAddTerm name pterm
  | (not . Set.null . Indices.free) pterm =
    error "Cannot define term containing free variables."
  | otherwise =
    modify dbTerms (Map.insert name pterm)   
    
dbTermName :: (Show Term, Fail.Can m) => Term -> Database -> m (String, [Term])
dbTermName (Lam {}) _ = Fail.here
dbTermName term _ 
  | (not . isFix . leftmost) term = Fail.here
dbTermName term (get dbTermNames -> name_list) = do
  (uni, name) <- Fail.choose (map unifiable name_list)
  
  -- Every free variable needs to have been bound, so we just check the 
  -- sorted list of indices is equal to the list of all indices
  let uni_list = sort (Map.toList uni)
  Fail.unless (map (fromEnum . fst) uni_list == [0..length uni_list - 1])
  
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
  lookupName = asks . dbTermName
  lookupType n = asks . dbGetType n
  
  lookupTerm name ty_args = 
    asks dbGetTerm
    where
    dbGetTerm = id
      . fmap (Type.monomorphic ty_args)
      . Map.lookup name
      . get dbTerms
  
newtype DBStateT m a 
  = DBStateT { runDBStateT :: StateT Database m a }
  deriving ( Monad, MonadTrans, MonadState Database )
  
type DBState = DBStateT Identity
  
evalEmptyT :: Monad m => DBStateT m a -> m a
evalEmptyT = flip evalStateT emptyDatabase . runDBStateT

evalEmpty :: DBState a -> a 
evalEmpty = runIdentity . evalEmptyT

mapDBStateT :: ()
  => (m (a, Database) -> n (b, Database)) 
  -> (DBStateT m a -> DBStateT n b)
mapDBStateT f = DBStateT . mapStateT f . runDBStateT

instance (Show Term, Monad m) => Read (DBStateT m) where
  lookupType n = State.gets . dbGetType n
  lookupName = State.gets . dbTermName
  
  lookupTerm name ty_args = do 
    mby_pterm <- State.gets (Map.lookup name . get dbTerms)
    case mby_pterm of
      Nothing -> return Nothing
      Just pterm -> do
        let term = Type.monomorphic ty_args pterm
        State.modify (addName term)
        return (Just term)
    where
    -- When we retrieve a term with a set of type arguments, we create an
    -- instance of that term with those arguments for fast lookup
    addName term = 
      modify dbTermNames addName
      where
      addName
        | (isFix . leftmost) inner_term = (++ [(inner_term, name')]) 
        | otherwise = id
        
      (_, inner_term) = flattenLam term
      
      name' 
        | length ty_args == 0 = name 
        | otherwise = name ++ "<" ++ args_s ++ ">"
        where
        args_s = intercalate ", " (map show ty_args)
      
      
instance (Show Term, Monad m) => Write (DBStateT m) where
  defineTerm n = State.modify . dbAddTerm n
  defineType n = State.modify . dbAddType n
  
instance Env.Write m => Env.Write (DBStateT m) where
  bindAt at b = mapDBStateT (Env.bindAt at b)
  matched t w = mapDBStateT (Env.matched t w)
  
instance Env.Read m => Env.Read (DBStateT m) where
  bindings = Trans.lift Env.bindings
  
instance Discovery.Tells m => Discovery.Tells (DBStateT m) where
  tell = Trans.lift . Discovery.tell

instance Discovery.Listens m => Discovery.Listens (DBStateT m) where
  listen = mapDBStateT (liftM swap . Discovery.listen)
    where
    swap :: ((a, b), c) -> ((a, c), b)
    swap ((x, y), z) = ((x, z), y)
