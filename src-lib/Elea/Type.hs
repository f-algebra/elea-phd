module Elea.Type
(
  Type (..), Ind (..), ConArg (..), Bind (..),
  name, constructors, boundLabel, boundType, 
  empty, unit, pair, bool, 
  equation, isEquation,
  returnType, argumentTypes,
  isInd, isFun, fromBase,
  unflatten, flatten, unfold, argumentCount,
  recursiveArgs, nonRecursiveArgs, recursiveArgIndices,
  isBaseCase, isRecursive, 
  makeAltBindings,
)
where

import Prelude ()
import Elea.Prelude
import Elea.Index
import qualified Elea.Monad.Failure as Fail

-- | An argument to a constructor.
data ConArg 
  -- | The inductive type we are defining.
  = IndVar 
  
  -- | A non-recursive type argument for the constructor.
  | ConArg !Type
  deriving ( Eq, Ord )
 
-- | @Ind@uctive type
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

-- | The 'empty' type, viz. the constructorless inductive type.
empty :: Ind
empty = Ind "empty" []

-- | The unit type, viz. the single parameterless constructor inductive type.
unit :: Ind
unit = Ind "unit" [("()", [])]

-- | Cartesian product.
pair :: Type -> Type -> Ind
pair a b = 
  Ind name [("pair", [ConArg a, ConArg b])]
  where
  name = "(" ++ show a ++ ", " ++ show b ++ ")"
  
-- | Just like a cartesian product, but with both parts of the same type.
-- This also has a special display rule in "Elea.Show".
equation :: Type -> Ind
equation a = 
  Ind name [("==", [ConArg a, ConArg a])]
  where
  name = "==[" ++ show a ++ "]"
  
isEquation :: Fail.Can m => Ind -> m Type
isEquation (Ind _ [("==", [ConArg x, ConArg y])]) 
  | x == y = return x
isEquation _ = Fail.here

bool :: Ind
bool = Ind "bool" [("True", []), ("False", [])]

  
-- Helpful functions

isInd :: Type -> Bool
isInd (Base _) = True
isInd _ = False

isFun :: Type -> Bool
isFun (Fun {}) = True
isFun _ = False

fromBase :: Type -> Ind
fromBase = inductiveType

flatten :: Type -> [Type]
flatten (Fun x r) = x : flatten r
flatten ty = [ty]

unflatten :: [Type] -> Type
unflatten = foldr1 Fun

returnType :: Type -> Type
returnType = last . flatten

argumentTypes :: Type -> [Type]
argumentTypes = init . flatten

argumentCount :: Type -> Int
argumentCount = pred . length . flatten

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
    
-- | Return the positions of the recursive arguments of a given constructor
recursiveArgs :: Ind -> Nat -> [Int]
recursiveArgs (Ind _ cons) n = id
  . assert (length cons > n)
  $ findIndices (== IndVar) con_args
  where
  (_, con_args) = cons !! enum n

-- | Return the de-Bruijn indices that will be bound to the recursive arguments
-- of a given constructor within a pattern match. 
-- For example, the second constructor of @ntree@ 
-- is @Cons: nat -> nlist -> nlist@:
-- > recursiveArgs nlist 1 == [1]
-- > recursiveArgIndices nlist 1 == [0]
recursiveArgIndices :: Ind -> Nat -> [Index]
recursiveArgIndices ind@(Ind _ cons) n = id
  . assert (length cons > n)
  . map enum
  . map (\x -> (arg_count - x) - 1)
  $ recursiveArgs ind n
  where
  -- The number of arguments this constructor has
  arg_count = length (snd (cons !! enum n))
  
-- | Returns the opposite indices to 'recursiveArgs'
nonRecursiveArgs :: Ind -> Nat -> [Int]
nonRecursiveArgs (Ind _ cons) n = id
  . assert (length cons > n)
  $ findIndices (/= IndVar) con_args
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
  
-- | Given a specific constructor index of an inductive type, return 
-- appropriate bindings for a pattern match on that constructor.
-- > makeAltBindings nlist 1 = [Bind "B0" nat, Bind "B1" nlist]
makeAltBindings :: Ind -> Nat -> [Bind]
makeAltBindings ind con_n =
  zipWith Bind arg_names arg_tys
  where
  Bind _ con_ty = unfold ind !! enum con_n
  arg_tys = init (flatten con_ty)
  arg_names = map (\n -> "B" ++ show n) [0..]
  
  
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

