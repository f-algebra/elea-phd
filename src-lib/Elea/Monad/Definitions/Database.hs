module Elea.Monad.Definitions.Database
(
  Database,
  empty,
  putType,
  getType,
  putTerm,
  getTerm,
  getName,
  getTerm',
  stateGetTerm,
)
where

import Elea.Prelude
import Elea.Term
import Elea.Type ( Polymorphic, Ind (..) )
import Elea.Unification ( Unifier )
import qualified Elea.Unification.Map as UMap
import qualified Elea.Type as Type
import qualified Elea.Index as Indices
import qualified Elea.Unification as Unifier
import qualified Elea.Monad.Env as Env
import qualified Elea.Monad.Failure.Class as Fail
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad.State.Class as State

data Database
  = Database { _dbTerms :: !(Map String (Polymorphic Term))
             , _dbTypes :: !(Map String (Polymorphic Ind))
             , _dbTermNames :: !(UMap.UMap Term String) }

mkLabels [ ''Database ]

empty :: Database
empty = Database mempty mempty UMap.empty
    
putType :: String -> Polymorphic Ind -> Database -> Database
putType name ty =
  modify dbTypes (Map.insert name ty)
  
getType :: Fail.Can m => String -> [Type] -> Database -> m Ind
getType name ty_args db = do
  p_ind <- Fail.mapLookup name (get dbTypes db)
  return (Type.monomorphic ty_args p_ind)
             
putTerm :: String -> Polymorphic Term -> Database -> Database
putTerm name pterm
 -- | (not . Set.null . Indices.free) pterm =
   -- error "Cannot define term containing free variables."
  | otherwise =
    modify dbTerms (Map.insert name pterm)   
    
getName :: Fail.Can m => Term -> Database -> m (String, [Term])
getName (Lam {}) _ = Fail.here
getName term _ 
  | (not . isFix) term = Fail.here
getName term (get dbTermNames -> name_map) = do
  (uni, name) <- UMap.lookup term' name_map
  
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
  
    
getTerm :: (Show Term, Fail.Can m) 
  => String -> [Type] -> Database -> m (Term, Database)
getTerm name ty_args db = do 
  pterm <- Fail.mapLookup name (get dbTerms db)
  let term = Type.monomorphic ty_args pterm
  return (term, addName term db)
  where
  -- When we retrieve a term with a set of type arguments, we create an
  -- instance of that term with those arguments for fast lookup
  addName term = 
    modify dbTermNames addName
    where
    addName
      | isFix inner_term = UMap.insert inner_term name' 
      | otherwise = id
      
    (_, inner_term) = flattenLam term
    
    name' 
      | length ty_args == 0 = name 
      | otherwise = name ++ "<" ++ args_s ++ ">"
      where
      args_s = intercalate ", " (map show ty_args)
      
stateGetTerm :: (Show Term, Fail.Can m, MonadState Database m) 
  => String -> [Type] -> m Term
stateGetTerm name ty_args = do
  (t, db') <- join (State.gets (getTerm name ty_args))
  State.put db'
  return t
  

getTerm' :: (Show Term, Fail.Can m) => String -> [Type] -> Database -> m Term
getTerm' name ty_args = liftM fst . getTerm name ty_args
