module Elea.Definition
(
  Definition,
  name, body, bindings, isRecursive, argumentRecursion,
)
where

import Elea.Prelude
import Elea.Term hiding ( name )
import qualified Elea.Name as Name
import qualified Elea.Type as Type

data ArgRecursion 
  -- | If the argument never changes on any recursive call
  = Constant 
  
  -- | If the argument structurally decreases at every recursive call
  | Decreasing 
  
  -- | If neither of the above hold
  | Accumulating

  
data Definition = Definition    
  { _name :: Name
  , _body :: Term 
  , _bindings :: [Bind]
  , _isRecursive :: Bool
  , _argumentRecursion :: [ArgRecursion] 
  }
  
mkLabels [ ''Definition ]

dtype :: Definition :-> Type
dtype = Name.typeOf . name

-- | Some assertions about the structure of definitions.
valid :: Definition -> Bool
valid def = arg_tys == arg_tys'
  where
  arg_tys = id
    . init
    . Type.flatten 
    $ get dtype def
    
  arg_tys' = 
    map (get Type.boundType) (get bindings def)

