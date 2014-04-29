module Elea.Type
(
  Type (..), 
  Ind (..), 
  ConArg (..), 
  Bind (..),
  Constructor (..),
  Schema (..), 
  
  ContainsTypes (..), mapTypes,
  
  name, boundLabel, boundType, 
  constructorOf, constructorIndex,
  subst, substMany, instantiate,
  empty, unit, bool, 
  true, false,
  equation, isEquation,
  returnType, argumentTypes,
  isInd, isFun, fromBase,
  unflatten, flatten, unfold, argumentCount,
  constructors, dropArgs,
  constructorType,
  recursiveArgs, nonRecursiveArgs, recursiveArgIndices,
  isBaseCase, isRecursive, 
  makeAltBindings,
  showWithArgs,
)
where

import Elea.Prelude
import Elea.Index ( Index )
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Failure.Class as Fail

type TyVar = String

-- | An argument to a constructor.
data ConArg 
  -- | The inductive type we are defining.
  = IndVar 
  
  -- | A non-recursive type argument for the constructor.
  | ConArg !Type
  deriving ( Eq, Ord )
  
type ConDef = (String, [ConArg])

  
-- | @Ind@uctive type
data Ind
  = Ind   { _name :: !String
          , _typeArgs :: ![Type]
          , _constructorDefs :: ![ConDef] }
  deriving ( Eq, Ord )
  
data Type 
  = Base  { inductiveType :: !Ind }
  
  | Var   { typeVar :: !TyVar }
  
  | Fun   { argument :: !Type
          , result :: !Type }
          
  deriving ( Eq, Ord )
  
-- | A type schema is a type with some forall quantified variables
data Schema
  = Schema  { _schemaVars :: ![TyVar]
            , _schemaType :: !Type }
  
-- | Binding a de-Bruijn index with a label and a type.
-- Something that would be used in a lambda abstraction
-- or pattern match variables.
data Bind 
  = Bind  { _boundLabel :: !String
          , _boundType :: !Type }
          
data Constructor
  = Constructor { _constructorOf :: !Ind
                , _constructorIndex :: !Nat }
  deriving ( Eq, Ord )
          
-- Equality ignores labels.
instance Eq Bind where
  (==) = (==) `on` _boundType
  
-- Inequality ignores labels.
instance Ord Bind where
  compare = compare `on` _boundType
  
  
-- | The prime type for 'Type', to allow us to use the "Elea.Foldable" code.
data Type' a 
  = Base' (Ind' a)
  | Var' String
  | Fun' a a
  deriving ( Functor, Foldable, Traversable )
  
data Ind' a 
  = Ind' String [a] [(String, [ConArg' a])]
  deriving ( Functor, Foldable, Traversable )
  
data ConArg' a 
  = IndVar'
  | ConArg' a
  deriving ( Functor, Foldable, Traversable )
  
type instance Fold.Base Type = Type'

projectConArg :: ConArg -> ConArg' Type 
projectConArg IndVar = IndVar'
projectConArg (ConArg ty) = ConArg' ty

embedConArg :: ConArg' Type -> ConArg 
embedConArg IndVar' = IndVar
embedConArg (ConArg' ty) = ConArg ty

projectInd :: Ind -> Ind' Type
projectInd (Ind n ts cs) = 
  Ind' n ts (map (second (map projectConArg)) cs)

embedInd :: Ind' Type -> Ind
embedInd (Ind' n ts cs) = 
  Ind n ts (map (second (map embedConArg)) cs)

instance Fold.Foldable Type where
  project (Base ind) = Base' (projectInd ind)
  project (Var x) = Var' x
  project (Fun t1 t2) = Fun' t1 t2
  
instance Fold.Unfoldable Type where
  embed (Base' ind) = Base (embedInd ind)
  embed (Var' x) = Var x
  embed (Fun' t1 t2) = Fun t1 t2
  
  
mkLabels [ ''Bind, ''Ind, ''Constructor, ''Schema ]


-- | Anything that contains types. Will only apply the function to the very
-- top level types within something. Does not recurse into types themselves,
-- hence the instance for 'Type' itself.
class ContainsTypes t where
  mapTypesM :: Monad m => (Type -> m Type) -> t -> m t
  
mapTypes :: ContainsTypes t => (Type -> Type) -> t -> t
mapTypes f = runIdentity . mapTypesM (return . f)


instance ContainsTypes Type where
  mapTypesM = ($)
  
instance ContainsTypes Bind where
  mapTypesM f (Bind name ty) =
    return (Bind name) `ap` f ty
    
instance ContainsTypes t => ContainsTypes (a, t) where
  mapTypesM f (n, t) = do
    t' <- mapTypesM f t
    return (n, t')
    
instance ContainsTypes t => ContainsTypes [t] where
  mapTypesM f = mapM (mapTypesM f)
    
instance ContainsTypes ConArg where
  mapTypesM _ IndVar = 
    return IndVar
  mapTypesM f (ConArg ty) =
    return ConArg `ap` f ty
  
instance ContainsTypes Ind where
  mapTypesM f (Ind name tys cons) = do
    tys' <- mapTypesM f tys
    cons' <- mapTypesM f cons
    return (Ind name tys' cons')
  
instance ContainsTypes Constructor where
  mapTypesM f = modifyM constructorOf (mapTypesM f)
  
  
subst :: ContainsTypes t => TyVar -> Type -> t -> t
subst var with = 
  mapTypes (Fold.transform sub)
  where
  sub (Var x) 
    | x == var = with
    | otherwise = Var x
  sub other = 
    other
    
substMany :: ContainsTypes t => [(TyVar, Type)] -> t -> t
substMany = flip (foldr (uncurry subst)) 

instantiate :: Schema -> [Type] -> Type
instantiate (Schema xs ty) tys = id
  . assert (length xs == nlength tys)
  $ substMany (zip xs tys) ty

-- | The 'empty' type, viz. the constructorless inductive type.
empty :: Ind
empty = Ind "empty" [] []

-- | The unit type, viz. the single parameterless constructor inductive type.
unit :: Ind
unit = Ind "unit" [] [("()", [])]
  
-- | Just like a cartesian product, but with both parts of the same type.
-- This also has a special display rule in "Elea.Show".
equation :: Type -> Ind
equation a = 
  Ind name [a] [("==", [ConArg a, ConArg a])]
  where
  name = "eq"
  
isEquation :: Constructor -> Bool
isEquation (Constructor (Ind "eq" [ty] [("==", [ConArg ty1, ConArg ty2])]) 0) = 
  ty1 == ty && ty2 == ty
isEquation _ = False

bool :: Ind
bool = Ind "bool" [] [("True", []), ("False", [])]

true, false :: Constructor
true = Constructor bool 0
false = Constructor bool 1

  
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

-- | Drops arguments from a type.
-- > dropArgs 2 (A -> B -> C -> D) = C -> D
dropArgs :: Nat -> Type -> Type
dropArgs 0 ty = ty
dropArgs n (Fun _ ty) = dropArgs (pred n) ty

unfold :: Ind -> [Bind]
unfold ind@(Ind _ _ cons) =
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


-- | All the constructors of an inductive type
constructors :: Ind -> [Constructor]
constructors ind@(Ind _ _ cons) = 
  map (Constructor ind . fst) ([0..] `zip` cons)
  
constructorType :: Constructor -> Type
constructorType (Constructor ind n) = id
  . get boundType
  . (!! n)
  $ unfold ind
    
    
-- | Return the positions of the recursive arguments of a given constructor
recursiveArgs :: Constructor -> [Int]
recursiveArgs (Constructor (Ind _ _ cons) n) = id
  . assert (length cons > n)
  $ findIndices (== IndVar) con_args
  where
  (_, con_args) = cons !! n

  
-- | Return the de-Bruijn indices that will be bound to the recursive arguments
-- of a given constructor within a pattern match. 
-- For example, the second constructor of @ntree@ 
-- is @Cons: nat -> nlist -> nlist@:
-- > recursiveArgs nlist 1 == [1]
-- > recursiveArgIndices nlist 1 == [0]
recursiveArgIndices :: Constructor -> [Index]
recursiveArgIndices con@(Constructor ind@(Ind _ _ cons) n) = id
  . assert (length cons > n)
  . map enum
  . map (\x -> (arg_count - x) - 1)
  $ recursiveArgs con
  where
  -- The number of arguments this constructor has
  arg_count = length (snd (cons !! n))
  
  
-- | Returns the opposite indices to 'recursiveArgs'
nonRecursiveArgs :: Constructor -> [Int]
nonRecursiveArgs (Constructor (Ind _ _ cons) n) = id
  . assert (length cons > n)
  $ findIndices (/= IndVar) con_args
  where
  (_, con_args) = cons `nth` enum n
  
  
-- | Whether a constructor has no recursive arguments
isBaseCase :: Constructor -> Bool
isBaseCase con = 
  -- A base case is one that has no recursive arguments
  recursiveArgs con == []
  
  
-- | Whether an inductive type has any recursive constructors.
isRecursive :: Ind -> Bool
isRecursive = all isBaseCase . constructors

  
-- | Given a specific constructor index of an inductive type, return 
-- appropriate bindings for a pattern match on that constructor.
-- > makeAltBindings nlist 1 = [Bind "B0" nat, Bind "B1" nlist]
makeAltBindings :: Ind -> Nat -> [Bind]
makeAltBindings ind con_n =
  zipWith Bind arg_names arg_tys
  where
  Bind _ con_ty = unfold ind `nth` enum con_n
  arg_tys = init (flatten con_ty)
  arg_names = map (\n -> "B" ++ show n) [0..]
  
  
showWithArgs :: String -> [Type] -> String
showWithArgs name tys
  | length tys == 0 = name
  | otherwise = name ++ "<" ++ intercalate ", " (map show tys) ++ ">"
  
instance Show Ind where
  show (Ind name tys cons) =
    showWithArgs name tys
  
instance Show ConDef where
  show (name, args) = 
    " | " ++ name ++ concatMap (\t -> " " ++ showArg t) args
    where
    showArg IndVar = name
    showArg (ConArg ty) = show ty
      
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
    
instance Show Constructor where
  show (Constructor (Ind _ tys cons) n) = 
    showWithArgs (fst (cons !! n)) tys

