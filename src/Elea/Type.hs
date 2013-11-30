module Elea.Type
(
  Type (..), Ind (..), ConArg (..), Bind (..),
  isInd, isFun,
  unflatten, flatten, unfold,
  empty, unit, nat, bool, natlist,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Monad.Error as Error

-- An argument to a constructor is either the inductive type we are defining
-- or another type.
data ConArg = IndVar | ConArg Type
  deriving ( Eq, Ord )
 
-- | "Ind"uctive type
data Ind 
  = Ind   { name :: !String
          , constructors :: ![(String, [ConArg])] }
  
data Type 
  -- | Base types are inductive data types.
  = Base  { inductiveType :: !Ind }
  
  -- | Function types.
  | Fun   { argument :: !Type
          , result :: !Type }
          
  deriving ( Eq, Ord )
  
-- | Binding a de-Bruijn index with a label and a type.
-- Something that would be used in a lambda abstraction
-- or pattern match variables.
data Bind 
  = Bind  { boundLabel :: !String
          , boundType :: !Type }
          
-- Equality ignores labels.
instance Eq Bind where
  (==) = (==) `on` boundType
  
instance Eq Ind where
  (==) = (==) `on` map snd . constructors
  
-- Inequality ignores labels.
instance Ord Bind where
  compare = compare `on` boundType
  
instance Ord Ind where
  compare = compare `on` map snd . constructors

-- Some built in types

empty = Ind "empty" []
unit = Ind "unit" [("()", [])]
bool = Ind "bool" [("True", []), ("False", [])]
nat = Ind "nat" [("0", []), ("Suc", [IndVar])]
natlist = Ind "nlist" [("Nil", []), ("Cons", [ConArg (Base nat), IndVar])]

-- Helpful functions

isInd :: Type -> Bool
isInd (Base _) = True
isInd _ = False

isFun :: Type -> Bool
isFun (Fun {}) = True
isFun _ = False

flatten :: Type -> [Type]
flatten (Fun x r) = x : flatten r
flatten ty = [ty]

unflatten :: [Type] -> Type
unflatten = foldr1 Fun

unfold :: Ind -> [Bind]
unfold ind@(Ind _ cons) =
  map unfoldCon cons
  where
  unfoldCon :: (String, [ConArg]) -> Bind
  unfoldCon (name, args) = 
    Bind name (unflatten (args' ++ [Base ind]))
    where
    args' :: [Type]
    args' = map unfoldArg args
    
    unfoldArg :: ConArg -> Type
    unfoldArg IndVar = Base ind
    unfoldArg (ConArg ty) = ty
  
instance Show Ind where
  show = name
  
instance Show Type where
  show (Base ind) = show ind
  show (Fun a r) = a_s ++ " -> " ++ r_s
    where
    a_s | ' ' `elem` a_s' = "(" ++ a_s' ++ ")"  
        | otherwise = a_s'
      where
      a_s' = show a
    r_s = show r
    
instance Show Bind where
  show (Bind name ty) =
    "(" ++ name ++ " : " ++ show ty ++ ")" 

