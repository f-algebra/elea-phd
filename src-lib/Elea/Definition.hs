module Elea.Definition
(
  Definition (..)
)
where

import Elea.Prelude
import Elea.Term
import Elea.Monad.Env ()
import qualified Elea.Type as Type

data ArgRecursion 
  -- | If the argument never changes on any recursive call
  = Constant 
  
  -- | If the argument structurally decreases at every recursive call
  | Decreasing 
  
  | Accumulating
  
  | Unknown


data Definition = 
  Definition  { name :: Name
              , body :: Term 
              , bindings :: [Bind]
              , isRecursive :: Bool
              , argRecursion :: [ArgRecursion] }

instance Type.ContainsTypes Definition where
  mapTypesM f def@(Definition { body = body, bindings = bs }) = do
    body' <- Type.mapTypesM f body
    bs' <- Type.mapTypesM f bs
    return (def { body = body', bindings = bs' })

