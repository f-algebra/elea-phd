module Elea.Name
(
  Name (..), typeOf, 
)
where

import Elea.Prelude 
import Elea.Type ( Type )
import qualified Elea.Type as Type
  
-- | Something for naming terms. Should be globally unique. 
-- We store types along with names for simplicity and efficiency.
data Name 
  = Name    { label :: !String
            , schema :: !Type.Schema 
            , args :: ![Type] }

typeOf :: Name -> Type
typeOf (Name { schema = schema, args = tys }) = 
  Type.instantiate schema tys
  
instance Eq Name where
  (==) = (==) `on` label
  
instance Ord Name where
  compare = compare `on` label
  
instance Show Name where
  show (Name { label = id, args = tys }) = 
    Type.showWithArgs id tys
  
