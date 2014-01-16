module Elea.Type
(
  Type (..), Ind (..), ConArg (..), Bind (..),
  name, constructors, boundLabel, boundType, 
  empty, returnType,
  isInd, isFun,
  unflatten, flatten, unfold,
  isRecursive, recursiveArgs, nonRecursiveArgs, 
  isBaseCase,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Monad.Error as Error

-- An argument to a constructor is either the inductive type we are defining
-- or another type.
data ConArg = IndVar | ConArg !Type
  deriving ( Eq, Ord )
 
-- | "Ind"uctive type
data Ind 
  = Ind   { _name :: !String
          , _constructors :: ![(String, [ConArg])] }
  
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
  = Bind  { _boundLabel :: !String
          , _boundType :: !Type }
          
-- Equality ignores labels.
instance Eq Bind where
  (==) = (==) `on` _boundType
  
instance Eq Ind where
  (==) = (==) `on` map snd . _constructors
  
-- Inequality ignores labels.
instance Ord Bind where
  compare = compare `on` _boundType
  
instance Ord Ind where
  compare = compare `on` map snd . _constructors
  
mkLabels [ ''Bind, ''Ind ]

-- | The 'empty' type. Made from the constructorless inductive type.
empty :: Type
empty = Base (Ind "empty" [])

  
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

returnType :: Type -> Type
returnType = last . flatten

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
    
-- | Return the indices of the recursive arguments of a given constructor
recursiveArgs :: Ind -> Nat -> [Int]
recursiveArgs (Ind _ cons) n =
  findIndices (== IndVar) con_args
  where
  (_, con_args) = cons !! enum n
  
-- | Returns the opposite indices to 'recursiveArgs'
nonRecursiveArgs :: Ind -> Nat -> [Int]
nonRecursiveArgs (Ind _ cons) n =
  findIndices (/= IndVar) con_args
  where
  (_, con_args) = cons !! enum n
  
-- | For a given inductive type, return whether the constructor at that 
-- index is a base case.
isBaseCase :: Ind -> Nat -> Bool
isBaseCase ind n = 
  -- A base case is one that has no recursive arguments
  recursiveArgs ind n == []
  
-- | Whether an inductive type has any recursive constructors.
isRecursive :: Ind -> Bool
isRecursive ind = id
  . any (not . isBaseCase ind . enum)
  $ [0..length (get constructors ind) - 1]
  
  
instance Show Ind where
  show = get name
  
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
    "(" ++ name ++ ": " ++ show ty ++ ")" 

