module Elea.Type
(
  Type (..), Ind (..), ConArg (..), 
  Bind (..), Constructor (..),
  
  ContainsTypes (..), mapTypes,
  HasType (..),
  
  indName, indConstructors, 
  bindLabel, bindType, 
  constructorOf, constructorIndex, constructorTyArgs,
  prop, tag, unit, tuple, bool, falsity, propTy,
  equation, isEquation, true, false,
  returnType, argumentTypes, dropArgs, split,
  isInd, isFun, fromBase,
  unflatten, flatten, unfold, argumentCount,
  recursiveArgs, nonRecursiveArgs, recursiveArgIndices,
  constructors,
  isBaseCase, isRecursive, 
  makeAltBindings,
  bindEq,
  assertEq,
  
  Polymorphic, 
  polymorphic, polymorphicM, 
  monomorphic, uninterpreted,
  typeArgs,
)
where 

import Elea.Prelude hiding ( get )
import Elea.Term.Index
import Elea.Term.Tag ( Tagged )
import Elea.Foldable.WellFormed ( WellFormed (..) )
import qualified Elea.Term.Tag as Tag
import qualified Elea.Foldable as Fold
import qualified Elea.Monad.Error.Assertion as Assert
import qualified Elea.Monad.Failure.Class as Fail

{-# ANN module "HLint: ignore Redundant id" #-}

-- | An argument to a constructor.
data ConArg 
  -- | The inductive type we are defining.
  = IndVar 
  
  -- | A non-recursive type argument for the constructor.
  | ConArg !Type
  deriving ( Eq, Ord )
 
-- | @Ind@uctive type
data Ind 
  = Ind   { _indName :: !String
          , _indConstructors :: ![(String, [ConArg])]
          , _indTyArgs :: [Type] }
  
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
  = Bind  { _bindLabel :: !String
          , _bindType :: !Type }
          
data Constructor
  = Constructor { _constructorOf :: !Ind
                , _constructorIndex :: !Nat
                , _constructorTyArgs :: [Type] }
  deriving ( Eq, Ord )
          
-- Equality ignores labels.
instance Eq Bind where
  (==) = (==) `on` _bindType
  
instance Eq Ind where
  (==) = (==) `on` map snd . _indConstructors
  
-- Inequality ignores labels.
instance Ord Bind where
  compare = compare `on` _bindType
  
instance Ord Ind where
  compare = compare `on` map snd . _indConstructors
  
  
-- | The prime type for 'Type', to allow us to use the "Elea.Foldable" code.
data Type' a 
  = Base' (Ind' a)
  | Fun' a a
  deriving ( Functor, Foldable, Traversable )
  
data ConArg' a 
  = IndVar'
  | ConArg' a
  deriving ( Functor, Foldable, Traversable )
  
data Ind' a 
  = Ind' String [(String, [ConArg' a])] [a]
  deriving ( Functor, Foldable, Traversable )
  
type instance Fold.Base Type = Type'

projectConArg :: ConArg -> ConArg' Type 
projectConArg IndVar = IndVar'
projectConArg (ConArg ty) = ConArg' ty

projectInd :: Ind -> Ind' Type
projectInd (Ind n cs ty_args) = 
  Ind' n (map (second (map projectConArg)) cs) ty_args

embedConArg :: ConArg' Type -> ConArg 
embedConArg IndVar' = IndVar
embedConArg (ConArg' ty) = ConArg ty

embedInd :: Ind' Type -> Ind
embedInd (Ind' n cs ty_args) = 
  Ind n (map (second (map embedConArg)) cs) ty_args

instance Fold.Foldable Type where
  project (Base ind) = Base' (projectInd ind)
  project (Fun t1 t2) = Fun' t1 t2
  
instance Fold.Unfoldable Type where
  embed (Base' ind) = Base (embedInd ind)
  embed (Fun' t1 t2) = Fun t1 t2
  
  
mkLabels [ ''Bind, ''Ind, ''Constructor ]


-- | Anything that contains types. Will only apply the function to the very
-- top level types within something. Does not recurse into types themselves,
-- hence the instance for 'Type' itself.
class ContainsTypes t where
  mapTypesM :: Monad m => (Type -> m Type) -> t -> m t
  
mapTypes :: ContainsTypes t => (Type -> Type) -> t -> t
mapTypes f = runIdentity . mapTypesM (return . f)


instance ContainsTypes Type where
  mapTypesM = ($)

instance ContainsTypes ConArg where
  mapTypesM _ IndVar = 
    return IndVar
  mapTypesM f (ConArg ty) =
    return ConArg `ap` f ty
    
instance ContainsTypes Bind where
  mapTypesM f (Bind name ty) =
    return (Bind name) `ap` f ty
  
instance ContainsTypes Ind where
  mapTypesM f (Ind name cons type_args) = do
    args' <- mapM (mapM (mapTypesM f)) args
    type_args' <- mapM (mapTypesM f) type_args
    return (Ind name (zip names args') type_args')
    where
    args :: [[ConArg]]
    (names, args) = unzip cons
    
instance ContainsTypes Constructor where
  mapTypesM f = return
    >=> modifyM constructorOf (mapTypesM f)
    >=> modifyM constructorTyArgs (mapM (mapTypesM f))
 
instance ContainsTypes a => ContainsTypes (Tagged a) where
  mapTypesM f = mapM (mapTypesM f)
  
class Show a => HasType a where
  get :: a -> Type
  
instance HasType Constructor where
  get = get . constructorBind
  
instance HasType a => HasType (Tagged a) where
  get = get . Tag.untag
  
instance HasType Bind where
  get = _bindType
  
  
-- | The 'empty' type, viz. the constructorless inductive type.
instance Empty Ind where
  empty = Ind "empty" [] []

instance Empty Type where
  empty = Base empty

-- | Huge hack but the type of fixed-point tags is just the empty type
tag :: Ind
tag = empty

-- | The unit type, viz. the single parameterless constructor inductive type.
unit :: Ind
unit = Ind "unit" [("()", [])] []

prop :: Ind 
prop = Ind "prop" [("ff", [])] []

propTy :: Type
propTy = Base prop

falsity :: Constructor
falsity = Constructor prop 0 []

-- | Cartesian n-product.
tuple :: [Type] -> Ind
tuple tys 
  | length tys > 1 = 
    Ind name [("tuple", map ConArg tys)] []
  where
  name = "(" ++ intercalate "," (map show tys) ++ ")"  

  
-- | Just like a cartesian product, but with both parts of the same type.
-- This also has a special display rule in "Elea.Show".
equation :: Type -> Constructor
equation a = 
  Constructor ind 0 []
  where
  name = "==[" ++ show a ++ "]"
  ind = Ind name [("==", [ConArg a, ConArg a])] []
  
isEquation :: Constructor -> Maybe Type
isEquation (Constructor (Ind name [("==", [ConArg ty1, ConArg ty2])] []) 0 []) 
  | take 3 name == "==[" && ty1 == ty2 = return ty1
isEquation _ = Nothing

bool :: Ind
bool = Ind "bool" [("True", []), ("False", [])] []

true, false :: Constructor
true = Constructor bool 0 []
false = Constructor bool 1 []

  
-- Helpful functions

isInd :: Type -> Bool
isInd (Base _) = True
isInd _ = False

isFun :: Type -> Bool
isFun Fun{} = True
isFun _ = False

fromBase :: Type -> Ind
fromBase = inductiveType

flatten :: Type -> [Type]
flatten (Fun x r) = x : flatten r
flatten ty = [ty]

unflatten :: [Type] -> Type
unflatten = foldr1 Fun

returnType :: Type -> Ind
returnType = snd . split

split :: Type -> ([Type], Ind)
split (Fun x y) = (x:xs, r) 
  where
  (xs, r) = split y
split (Base ind) = ([], ind)

argumentTypes :: Type -> [Type]
argumentTypes = fst . split

argumentCount :: Type -> Nat
argumentCount = pred . nlength . flatten

-- | Drops arguments from a type.
-- > dropArgs 2 (A -> B -> C -> D) = C -> D
dropArgs :: Nat -> Type -> Type
dropArgs 0 ty = ty
dropArgs n (Fun _ ty) = dropArgs (pred n) ty
dropArgs _ _ = error "Type does not have enough arguments"

unfold :: Ind -> [Bind]
unfold ind@(Ind _ cons _) =
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
constructors ind@(Ind _ cons ty_args) =
  map (\n -> Constructor ind n ty_args) (range cons)
  
constructorType :: Constructor -> Type
constructorType (Constructor ind n _) = id
  . _bindType
  . (!! n)
  $ unfold ind
    
constructorBind :: Constructor -> Bind
constructorBind (Constructor ind n _) = 
  unfold ind !! n

    
-- | Return the positions of the recursive arguments of a given constructor
recursiveArgs :: Constructor -> [Nat]
recursiveArgs (Constructor (Ind _ cons _) n _) = id
  . Assert.assert "constructor index out of range" (nlength cons > n)
  . map enum
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
recursiveArgIndices con@(Constructor ind@(Ind _ cons _) n _) = id
  . Assert.assert "constructor index out of range" (nlength cons > n)
  . map enum
  . invert
  $ recursiveArgs con
  
  
-- | Returns the opposite indices to 'recursiveArgs'
nonRecursiveArgs :: Constructor -> [Int]
nonRecursiveArgs (Constructor (Ind _ cons _) n _) = id
  . Assert.assert "constructor index out of range" (elength cons > n)
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
isRecursive = not . all isBaseCase . constructors

  
-- | Given a specific constructor index of an inductive type, return 
-- appropriate bindings for a pattern match on that constructor.
-- > makeAltBindings nlist 1 = [Bind "b0" nat, Bind "b1" nlist]
makeAltBindings :: Constructor -> [Bind]
makeAltBindings (Constructor ind con_n _) =
  zipWith Bind arg_names arg_tys
  where
  Bind _ con_ty = unfold ind `nth` enum con_n
  arg_tys = init (flatten con_ty)
  arg_names = map (printf "b%d") ([0..] :: [Int])
  
  
-- | Things parameterised by types. 
-- Doesn't correspond to anywhere on the lambda cube, these are just macros
-- which allow for easier monomorphic typing. Actual terms are always 
-- monomorphic, but we can use these macros to define /polymorphic/ functions
-- and types.
data Polymorphic a
  = Poly [String] a
  deriving ( Functor )
  
instance Indexed a => Indexed (Polymorphic a) where
  free (Poly _ a) = free a
  shift f (Poly ns a) = Poly ns (shift f a)
  
typeArgCount :: Polymorphic t -> Nat
typeArgCount (Poly ns _) = elength ns
  
-- | Make a polymorphic instance of something which contains types.
-- Also propagates a monad, because that's always useful.
polymorphicM :: (Monad m, ContainsTypes t) 
  -- | The type argument names
  => [String] 
  -- | The function which when given the type arguments returns the 
  -- monomorphic object
  -> ([Type] -> m t)
  -> m (Polymorphic t)
polymorphicM arg_ns make = id
  . liftM (Poly arg_ns)
  . make 
  $ makePolyTyArgs arg_ns
    
polymorphic :: ContainsTypes t => [String] -> ([Type] -> t) -> Polymorphic t
polymorphic ns f = 
  runIdentity (polymorphicM ns (return . f))

makePolyTyArgs :: [String] -> [Type]
makePolyTyArgs arg_names = 
  map Base inds
  where
  -- We use specially named empty inductive types to mark type arguments
  inds = map makeInd [0..length arg_names - 1]
  makeInd i = Ind ("$" ++ show i) [] []

typeArgs :: Polymorphic a -> [Type]
typeArgs (Poly arg_names _) =
  makePolyTyArgs arg_names
  
-- | Apply type arguments to a polymorphic instance to get a monomorphic one
monomorphic :: (Show t, ContainsTypes t) => [Type] -> Polymorphic t -> t
monomorphic args poly@(Poly ns _)
  | length ns /= length args = 
    error 
       $ "Tried to make a monomorphic instance of " ++ show poly
       ++ " supplying types " ++ show args
       ++ " for variables " ++ show ns
monomorphic args poly@(Poly _ t) =
  mapTypes (Fold.transform applyArgs) t
  where
  applyArgs (Base (Ind ('$':idx) [] [])) = 
    args `nth` read idx
  applyArgs other = other
  
-- | Make a monomorphic instance of a polymorphic object using uninterpreted
-- type variables (we just use empty inductive types for this).
uninterpreted :: (Show t, ContainsTypes t) => Polymorphic t -> t
uninterpreted poly@(Poly ns _) = 
  monomorphic (map Base inds) poly
  where
  inds = map (\n -> Ind n [] []) ns
  
  
instance Show Ind where
  show (Ind name cons ty_args) = 
    name ++ showTyArgs ty_args
    where
    showCon :: (String, [ConArg]) -> String
    showCon (con_name, args) = 
      " | " ++ con_name ++ concatMap (\t -> " " ++ showArg t) args
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
    
instance (ContainsTypes a, Show a) => Show (Polymorphic a) where
  show p@(Poly ns _) =
    "forall " ++ intercalate " " ns ++ " -> " 
    ++ show (uninterpreted p)
    
instance Show Constructor where
  show (Constructor (Ind _ cons _) n ty_args) = 
    fst (cons !! n) ++ showTyArgs ty_args

showTyArgs :: [Type] -> String
showTyArgs [] = ""
showTyArgs ty_args = printf "<%s>" (intercalate "," (map show ty_args))

deriving instance Show ConArg

instance PrintfArg Ind where
  formatArg = formatString . show

instance PrintfArg Bind where
  formatArg = formatString . show

instance PrintfArg Constructor where
  formatArg = formatString . show

instance PrintfArg Type where
  formatArg = formatString . show

-- TODO turn Eq into bindEq such that none of the checks break
bindEq :: Bind -> Bind -> Bool
bindEq (Bind x ty) (Bind x' ty') = 
  x == x' && ty == ty'

assertEq :: (PrintfArg a, HasType a) => a -> a -> Assert.Assert
assertEq x y = id
  . Assert.augment (printf "comparing types of %b and %b" x y)
  $ Assert.equal (get x) (get y)

instance WellFormed Constructor where
  assert (Constructor (Ind _ _ ty_args) _ ty_args') = id
    . Assert.augment "Constructor type arguments must match those of its inductive type"
    $ Assert.equal ty_args ty_args'
