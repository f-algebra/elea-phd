module Elea.Type
(
  Poly (..),
  Inst (..),
  Type (..), 
  Ind (..), 
  ConArg (..), 
  Bind (..),
  Constructor (..),
  TyVar,
  Typed (..), specify,
  HasType (..),
  ContainsTypes (..),
  mapTypes,
  subst, substMany, instantiate,
  apply, applyM, wrap,
  empty, unit, bool,
  true, false,
  equation, isEquation,
  returnType, argumentTypes,
  isInd, isFun, fromBase,
  unflatten, flatten, unfold, argumentCount,
  constructors, dropArgs,
  constructorArgCount,
  recursiveArgs, nonRecursiveArgs, recursiveArgIndices,
  isBaseCase, isRecursive, 
  makeAltBindings,
)
where

import Elea.Prelude hiding ( product, get )
import Elea.Index ( Index )
import qualified Elea.Index as Indices
import qualified Elea.Foldable as Fold
import qualified Elea.Bifoldable as Bifold
import qualified Elea.Monad.Failure.Class as Fail

type TyVar = String

-- | @Poly a@ is something of type @a@, with a set of type variables
-- quantified over it. Inductive types, term definitions and constructors
-- are examples of things which could have polymorphic instances.
-- The underlying object type is the same for both polymorphic 
-- and monomorphic versions.
data Poly a
  = Forall  { polyVars :: ![TyVar]
            , polyObj :: !a } 
  deriving ( Eq, Ord, Functor, Foldable, Traversable )
  
-- | A monomorphic instance of an object. This object has had type variables 
-- replaced by a list of types. The underlying object type is the same
-- so this wrapper is merely symbolic of the fact that
-- this is a monomorphic instance of a polymorphic object.
data Inst a
  = Inst    { instArgs :: ![Type]
            , instObj :: !a }
  deriving ( Eq, Ord, Functor, Foldable, Traversable )


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
  = Ind   { typeName :: !String
          , constructorDefs :: ![ConDef] }
  deriving ( Eq, Ord )
  
  
data Type 
  = Base  { inductiveType :: !(Inst Ind) }
  
  | Var   { typeVar :: !TyVar }
  
  | Fun   { argument :: !Type
          , result :: !Type }
          
  deriving ( Eq, Ord )
  
  
-- | Binding a de-Bruijn index with a label and a type.
-- Something that would be used in a lambda abstraction
-- or pattern match variables.
data Bind 
  = Bind  { bindLabel :: !String
          , bindType :: !Type }
          
data Constructor
  = Constructor { constructorOf :: !Ind
                , constructorIndex :: !Nat }
  deriving ( Eq, Ord )
  
data Typed a 
  = Typed     { typedObj :: !a
              , typedType :: !Type }
              
specify :: a -> Type -> Typed a
specify = Typed

-- | The typing is just an annotation, so we ignore it for (in)equality.
instance Eq a => Eq (Typed a) where
  (==) = (==) `on` typedObj
  
instance Ord a => Ord (Typed a) where
  compare = compare `on` typedObj

          
-- Equality ignores labels.
instance Eq Bind where
  (==) = (==) `on` bindType
  
-- Inequality ignores labels.
instance Ord Bind where
  compare = compare `on` bindType

  
-- | The prime type for 'Type', to allow us to use the "Elea.Foldable" code.
data Type' a 
  = Base' (Inst' (Ind' a) a)
  | Var' String
  | Fun' a a
  
type instance Fold.Base Type = Type'
  
instance Functor Type' where
  fmap f (Base' (Inst' ts a)) =
    Base' (Inst' (map f ts) (fmap f a))
  fmap _ (Var' x) = Var' x
  fmap f (Fun' x1 x2) = Fun' (f x1) (f x2)
  
  
instance Fold.Foldable Type where
  project (Base (Inst ts pind)) = 
    Base' (Inst' ts (Bifold.project pind))
  project (Var x) = Var' x
  project (Fun t1 t2) = Fun' t1 t2
  
instance Fold.Unfoldable Type where
  embed (Base' (Inst' ts pind)) =
    Base (Inst ts (Bifold.embed pind))
  embed (Var' x) = Var x
  embed (Fun' t1 t2) = Fun t1 t2
  
    
data Ind' a 
  = Ind' String [(String, [ConArg' a])]
  deriving ( Functor, Foldable, Traversable )
  
type instance Fold.Base Ind = Ind'
  
instance Bifold.Bifoldable Ind where
  type Inner Ind = Type
  
  project (Ind n cs) = 
    Ind' n (map (second (map Bifold.project)) cs)
    
  embed (Ind' n cs) = 
    Ind n (map (second (map Bifold.embed)) cs)
    
  
data ConArg' a 
  = IndVar'
  | ConArg' a
  deriving ( Functor, Foldable, Traversable )
  
type instance Fold.Base ConArg = ConArg'
  
instance Bifold.Bifoldable ConArg where
  type Inner ConArg = Type

  project IndVar = IndVar'
  project (ConArg ty) = ConArg' ty

  embed IndVar' = IndVar
  embed (ConArg' ty) = ConArg ty
  
  
data Inst' a b
  = Inst' [b] a
  deriving ( Functor, Foldable, Traversable )
  

class ContainsTypes t where
  mapTypesM :: Monad m => (Type -> m Type) -> t -> m t
  
mapTypes :: ContainsTypes t => (Type -> Type) -> t -> t
mapTypes f = runIdentity . mapTypesM (Identity . f)

instance ContainsTypes Ind where
  mapTypesM = Bifold.mapM
  
instance ContainsTypes ConArg where
  mapTypesM = Bifold.mapM
  
instance ContainsTypes t => ContainsTypes (Poly t) where
  mapTypesM f (Forall xs t) =
    return (Forall xs) `ap` mapTypesM f t
  
instance ContainsTypes t => ContainsTypes (Inst t) where
  mapTypesM f (Inst ts t) = do
    ts' <- mapM f ts
    t' <- mapTypesM f t
    return (Inst ts' t')
    
instance ContainsTypes Bind where
  mapTypesM f (Bind lbl t) =
    return (Bind lbl) `ap` f t

instance ContainsTypes t => ContainsTypes [t] where
  mapTypesM f = mapM (mapTypesM f)

instance ContainsTypes Constructor where
  mapTypesM f (Constructor iind n) =
    return (\i -> Constructor i n) `ap` mapTypesM f iind  
    
instance ContainsTypes (Typed a) where
  mapTypesM f (Typed x ty) = 
    return (Typed x) `ap` f ty
    
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

  
-- | Abstract something over type arguments. May capture variables by accident,
-- so use carefully.
forall :: Nat -> ([TyVar] -> a) -> Poly a
forall n mk = Forall ty_vars (mk ty_vars)
  where
  ty_vars = map pure (take n ['a'..'z'])
  

-- | Create an instance of a polymorphic object by substituting
-- applied type arguments for type variables.
instantiate :: ContainsTypes a => [Type] -> Poly a -> Inst a
instantiate ts (Forall xs t) = 
  Inst ts (substMany (xs `zip` ts) t)

apply :: ContainsTypes b => (a -> Poly b) -> Inst a -> Inst b
apply f (Inst ts a) = 
  instantiate ts (f a)

applyM :: (Monad m, ContainsTypes b) 
  => (a -> m (Poly b)) -> Inst a -> m (Inst b)
applyM f (Inst ts a) = 
  liftM (instantiate ts) (f a)
  
wrap :: ContainsTypes a => Poly a -> (Inst a -> b) -> Poly b
wrap pa@(Forall xs a) make = id
  . Forall xs 
  . make
  $ instantiate (map Var xs) pa
  
  
-- | The 'empty' type, viz. the constructorless inductive type.
empty :: Inst Ind
empty = Inst [] (Ind "empty" [])

-- | The unit type, viz. the single parameterless constructor inductive type.
unit :: Inst Ind
unit = Inst [] (Ind "unit" [("()", [])])
  
-- | Just like a cartesian product, but with both parts of the same type.
-- This also has a special display rule in "Elea.Show".
equation :: Poly Ind
equation = 
  forall 1 $ \[a] -> (Ind "eq" [("==", [ConArg (Var a), ConArg (Var a)])])
  
isEquation :: Inst Constructor -> Bool
isEquation icon@(Inst [ty] _) =
  icon == fmap (head . constructors) (instantiate [ty] equation)
 
isEquation _ = False

bool :: Ind
bool = Ind "bool" [("True", []), ("False", [])]

true, false :: Inst Constructor
true = Inst [] (Constructor bool 0)
false = Inst [] (Constructor bool 1)

  
-- Helpful functions

isInd :: Type -> Bool
isInd (Base _) = True
isInd _ = False

isFun :: Type -> Bool
isFun (Fun {}) = True
isFun _ = False

fromBase :: Type -> Inst Ind
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

unfold :: Inst Ind -> [Bind]
unfold iind@(Inst _ (Ind _ cons)) =
  map unfoldCon cons
  where
  unfoldCon :: (String, [ConArg]) -> Bind
  unfoldCon (name, args) = 
    Bind name (unflatten (args' ++ [Base iind]))
    where
    args' :: [Type]
    args' = map unfoldArg args
    
    unfoldArg :: ConArg -> Type
    unfoldArg IndVar = Base iind
    unfoldArg (ConArg ty) = ty
    
constructorBind :: Inst Constructor -> Bind
constructorBind (Inst tys (Constructor ind n)) = 
  unfold (Inst tys ind) !! n


-- | All the constructors of an inductive type
constructors :: Ind -> [Constructor]
constructors ind = 
  map (Constructor ind) [0..con_count - 1]
  where
  con_count = nlength (constructorDefs ind)
    
constructorArgCount :: Constructor -> Nat
constructorArgCount (Constructor (Ind _ cons) n) =
  length (snd (cons !! n))
    
-- | Return the positions of the recursive arguments of a given constructor
recursiveArgs :: Constructor -> [Int]
recursiveArgs (Constructor (Ind _ cons) n) = id
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
recursiveArgIndices con@(Constructor (Ind _ cons) n) = id
  . assert (length cons > n)
  . map enum
  . map (\x -> (arg_count - x) - 1)
  $ recursiveArgs con
  where
  -- The number of arguments this constructor has
  arg_count = length (snd (cons !! n))
  
  
-- | Returns the opposite indices to 'recursiveArgs'
nonRecursiveArgs :: Constructor -> [Int]
nonRecursiveArgs (Constructor (Ind _ cons) n) = id
  . assert (length cons > n)
  $ findIndices (/= IndVar) con_args
  where
  (_, con_args) = cons !! n
  
  
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
makeAltBindings :: Inst Constructor -> [Bind]
makeAltBindings (Inst ty_args (Constructor ind con_n)) =
  zipWith Bind arg_names arg_tys
  where
  Bind _ con_ty = unfold (Inst ty_args ind) !! con_n
  arg_tys = init (flatten con_ty)
  arg_names = map (\n -> "B" ++ show n) [0..]
  
  
-- | Things that have a closed, always correct type. 
-- This is not terms, they could be badly typed, or require a type environment
-- to be typeable. This is for things like constructors, or names.
class HasType a where
  get :: a -> Type
  
instance HasType (Inst Constructor) where
  get = get . constructorBind
  
instance HasType Bind where
  get = bindType
  
instance HasType (Inst (Typed a)) where
  get = get . instObj
  
instance HasType (Typed a) where
  get = typedType
  

instance Show (Inst String) where
  show (Inst [] a) = a
  show (Inst ts a) = 
    a ++ "<" ++ intercalate ", " (map show ts) ++ ">"
  
instance Show Ind where
  show = show . typeName
  
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
    
instance Show (Inst Ind) where
  show = show . fmap show
    
instance Show (Inst Constructor) where
  show (Inst ts (Constructor (Ind _ cons) n)) = 
    show (Inst ts this_con)
    where
    this_con = fst (cons !! n)


