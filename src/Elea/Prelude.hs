module Elea.Prelude
       ((++), (!!), concat, intercalate, map, void, concatMap,
        concatMapM, partitionM, concatEndos, concatEndosM, fromJustT, anyM,
        allM, findM, sortWith, deleteIndices, minimalBy, nubOrd, elemOrd,
        intersectOrd, countOrd, fromRight, fromLeft, setAt, firstM,
        takeIndices, isNub, foldl1M, seqStr, strSeq, isLeft, isRight,
        modifyM, removeAt, insertAt, enum, elength, nlength, range, indent,
        indentBy, debugNth, arrowSum, supremum, (|>), ($>), replaceAt,
        Maximum(..), Minimum(..), sconcatMap, length, maximum, maximum1,
        invert, intersects, liftMaybe, maybeT, nth, drop, take, screen,
        isSubsequenceOf, evalWriter, evalWriterT, removeAll, tracE,
        findIndicesM, trace, error, errorf,
        Empty (..), Runnable (..), run, readf,
        __trace__,
        module X)
where

import Prelude as X
       hiding (mapM, foldl, foldl1, mapM_, minimum, maximum, sequence_,
               zip, zipWith, length, drop, take, foldr, foldr1, error,
               sequence, Maybe(..), maybe, all, any, elem, product, and, concat,
               notElem, or, concatMap, sum, (++), map, (.), id, (!!))
import Control.Category as X ((.), id)
import Control.Arrow as X
       (Arrow(..), (>>>), (<<<), (&&&), (***), first, second, Kleisli(..),
        runKleisli)
import Control.Applicative as X hiding (empty)
import Control.Monad as X
       (liftM, ap, replicateM, join, zipWithM_, zipWithM, filterM, when,
        unless, guard, (>=>), (<=<), (>>), MonadPlus(..))
import Control.Monad.Trans as X (MonadTrans(..), lift, liftIO)
import Control.Monad.State as X
       (evalStateT, execState, runState, evalState, MonadState, State(..),
        StateT(..), mapStateT)
import Control.Monad.Reader as X
       (MonadReader(..), Reader(..), ReaderT(..), asks, runReader,
        mapReaderT, withReaderT, withReader)
import Control.Monad.Writer as X
       (execWriter, runWriter, execWriterT, mapWriterT, MonadWriter(..),
        Writer(..), WriterT(..), censor, listens)
import Control.Monad.List as X (ListT(..))
import Control.Monad.Trans.Maybe as X
import Control.Monad.RWS.Lazy as X
       (RWS(..), RWST(..), execRWS, evalRWS, runRWS)
import Control.Monad.Identity as X (Identity(..))
import Control.Monad.Trans.Identity as X
       (IdentityT(..), mapIdentityT)
import Control.Monad.Trans.Either as X (EitherT(..), mapEitherT)
import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Nat as X (Nat, CoNat)
import Data.Label as X ((:->), get, set, modify, mkLabels, lens)
import Data.Maybe as X
import Data.Either as X (lefts, rights, partitionEithers)
import Data.Monoid as X hiding (Sum, All, (<>), Alt)
import Data.Semigroup as X (Semigroup(..))
import Data.Map.Strict as X (Map)
import Data.Sequence as X (Seq)
import Data.Set as X (Set, (\\))
import Data.IntSet as X (IntSet)
import Data.Traversable as X
import Data.Foldable as X
       hiding (concat, concatMap, maximum, minimum, length)
import Data.List as X
       (intersperse, unfoldr, partition, isPrefixOf, isSuffixOf,
        isInfixOf, sort, sortBy, findIndex, delete, elemIndices, intersect,
        union, transpose, groupBy, subsequences, isSuffixOf, deleteBy,
        findIndices, elemIndex)
import Data.IORef as X
import Data.Char as X
       (isAlpha, isDigit, isAlphaNum, isSpace, chr, ord)
import Data.IntMap as X (IntMap)
import Data.Function as X (on)
import Data.Text as X (Text)
import Data.String as X
import Data.Generics.Str as X
import Data.Key as X (Zip(..))
import Data.Proxy as X
import System.IO.Unsafe as X
import Text.Printf as X (printf, PrintfArg (..), formatString, formatInt)
import Text.Read as X ( Read (..) )
import GHC.Stack as X ( errorWithStackTrace, CallStack, showCallStack )

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Prelude as Pre
import qualified Data.Sequence as Seq
import qualified Data.Label.Partial as Partial
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Nat as Nat
import qualified Debug.Trace as Debug

{-# ANN module "HLint: ignore Redundant id" #-}

infixr 6 ++

{-# INLINE __trace__ #-}
__trace__ :: Bool
#ifdef TRACE
__trace__ = True
#else
__trace__ = False
#endif


(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

($>) :: Monad m => m a -> (a -> b) -> m b
($>) = flip liftM 

void :: Functor f => f a -> f ()
void = fmap (const ())

(++) :: Monoid m => m -> m -> m
(++) = mappend

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

concat :: Monoid m => [m] -> m
concat = mconcat

sconcatMap :: (Semigroup m, Foldable f) => (a -> m) -> f a -> m
sconcatMap f = sconcat . map f . NonEmpty.fromList . toList
  
instance Monad m => Monoid (Kleisli m a a) where
  mempty = arr id
  mappend = (>>>)

{-# ANN concatMap "HLint: ignore Use concatMap" #-}
concatMap :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
concatMap f = concat . map f . toList

concatMapM :: (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
concatMapM f = liftM concat . mapM f . toList

intercalate :: Monoid m => m -> [m] -> m
intercalate x = concat . intersperse x

length :: Foldable f => f a -> Int
length = Pre.length . toList

elength :: (Foldable f, Enum e) => f a -> e
elength = enum . length

nlength :: Foldable f => f a -> Nat
nlength = elength

range :: Foldable f => f a -> [Nat]
range xs = map enum [0..length xs - 1]

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = foldrM f' ([], [])  
  where
  f' a (xs, ys) = do
    p <- f a
    return $
      if p then (a : xs, ys) else (xs, a : ys)
      
fromJustT :: Monad m => MaybeT m a -> m a
fromJustT = liftM fromJust . runMaybeT

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b 
maybeT x f m = do
  mby_a <- runMaybeT m
  maybe x f mby_a

anyM :: (Monad f, Traversable t) => (a -> f Bool) -> t a -> f Bool
anyM f = liftM or . mapM f

allM :: (Monad f, Traversable t) => (a -> f Bool) -> t a -> f Bool
allM f = liftM and . mapM f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
  found <- p x
  if found
    then return (Just x)
    else findM p xs
    
-- | Returns the first element which is 'Just'. 
-- Ignores all monadic effects after this element.
firstM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstM [] = return Nothing
firstM (mx:mxs) = do
  mby_x <- mx
  maybe (firstM mxs) (return . Just) mby_x
  
foldl1M :: (Monad m, Foldable f) => (a -> a -> m a) -> f a -> m a
foldl1M f (toList -> xs) = foldlM f (head xs) (tail xs)
    
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (compare `on` f)

concatEndos :: Foldable f => f (a -> a) -> a -> a
concatEndos = appEndo . mconcat . map Endo . toList

concatEndosM :: (Monad m, Foldable f) => f (a -> m a) -> a -> m a
concatEndosM = runKleisli . mconcat . map Kleisli . toList

minimalBy :: (a -> a -> Ordering) -> [a] -> [a]
minimalBy _ [] = []
minimalBy ord xs = y : takeWhile ((== EQ) . ord y) ys
  where (y:ys) = sortBy ord xs

nubOrd :: Ord a => [a] -> [a]
nubOrd = reverse . fst . foldl nubby ([], Set.empty)
  where
  nubby (acc, set) x 
    | x `Set.member` set = (acc, set)
    | otherwise = (x:acc, Set.insert x set)

isNub :: forall a . Ord a => [a] -> Bool
isNub = isJust . foldrM dupM Set.empty
  where
  dupM :: a -> Set a -> Maybe (Set a)
  dupM x xs = do
    guard (not $ Set.member x xs)
    return (Set.insert x xs)
  
elemOrd :: Ord a => a -> [a] -> Bool
elemOrd x = Set.member x . Set.fromList

countOrd :: Ord a => a -> [a] -> Int
countOrd n = count . sort
  where
  count [] = 0
  count (x:xs) =
    case x `compare` n of
      LT -> count xs
      EQ -> 1 + count xs
      GT -> 0

intersectOrd :: Ord a => [a] -> [a] -> [a]
intersectOrd xs ys = Set.toList 
  $ Set.intersection (Set.fromList xs) (Set.fromList ys)
  
-- | Delete a set of indices from a list
deleteIndices :: [Int] -> [a] -> [a]
deleteIndices is = d 0 (sort is)
  where
  d :: Int -> [Int] -> [a] -> [a]
  d _ [] xs = xs
  d _ _ [] = []
  d i (j:js) (x:xs)
    | i == j = d (i + 1) js xs
    | otherwise = x : d (i + 1) (j:js) xs
    
-- | Only return the given indices of a list
takeIndices :: [Int] -> [a] -> [a]
takeIndices is = t 0 (sort is)
  where
  t :: Int -> [Int] -> [a] -> [a]
  t _ [] _ = []
  t _ _ [] = []
  t i (j:js) (x:xs) 
    | i == j = x : t (i + 1) js xs
    | otherwise = t (i + 1) (j:js) xs

fromRight :: Either a b -> b
fromRight (Right b) = b

fromLeft :: Either a b -> a
fromLeft (Left a) = a

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False

setAt :: Int -> a -> [a] -> [a]
setAt _ x [] = [x]
setAt 0 x xs = x : tail xs
setAt i x (y:ys) = y : setAt (i - 1) x ys

seqStr :: Seq a -> Str a
seqStr =  listStr . toList

strSeq :: Str a -> Seq a
strSeq = Seq.fromList . strList

modifyM :: Monad m => (f :-> a) -> (a -> m a) -> f -> m f
modifyM r g f = do
  x <- g (get r f)
  return (set r x f)
   
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x:ys
insertAt n x (y:ys) = y : insertAt (n-1) x ys
insertAt _ _ [] = 
  error "Can't insert past the end of a list"
  
removeAt :: Nat -> [a] -> [a]
removeAt _ [] = error "Can't remove past the end of a list"
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs

removeAll :: Set Nat -> [a] -> [a]
removeAll (Set.toAscList -> is) x = 
  foldr removeAt x is

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 x (y:ys) = x:ys
replaceAt n x (y:ys) = y: replaceAt (n-1) x ys
replaceAt _ _ [] = 
  error "Can't replace past the end of a list"
  
enum :: (Enum a, Enum b) => a -> b
enum = toEnum . fromEnum

indentBy :: Int -> String -> String
indentBy n [] = []
indentBy n ('\n':cs) = '\n' : replicate n ' ' ++ indentBy n cs
indentBy n (c:cs) = c : indentBy n cs
  
indent :: String -> String
indent = indentBy 2

(!!) :: Enum e => [a] -> e -> a
xs !! e = (Pre.!!) xs (fromEnum e)

debugNth :: String -> [a] -> Int -> a
debugNth msg xs n 
  | elength xs <= n = error msg
  | otherwise = xs !! n
  
nth :: [a] -> Int -> a
nth xs n = 
  debugNth ("Index too large: " ++ show (length xs) ++ " > " ++ show n) xs n
  
arrowSum :: MonadPlus m => [a -> m b] -> a -> m b
arrowSum ms x = msum (map ($ x) ms)

drop :: Enum e => e -> [a] -> [a]
drop = Pre.drop . fromEnum

take :: Enum e => e -> [a] -> [a]
take = Pre.take . fromEnum

supremum :: Enum a => Set a -> a
supremum set 
  | Set.null set = toEnum 0
  | otherwise = succ . head . Set.toDescList $ set

newtype Minimum a = Minimum { getMinimum :: a }
  deriving ( Eq, Ord, Enum, Num )

instance Ord a => Semigroup (Minimum a) where
  x <> y | x <= y = x
         | otherwise = y
 
newtype Maximum a = Maximum { getMaximum :: a }
  deriving ( Eq, Ord, Enum, Num )

instance Ord a => Semigroup (Maximum a) where
  x <> y | x >= y = x
         | otherwise = y
         
instance Monoid (Maximum Nat) where
  mappend = (<>)
  mempty = toEnum 0
  
instance Monoid (Minimum CoNat) where
  mappend = (<>)
  mempty = Minimum Nat.omega
  
intersects :: Ord a => Set a -> Set a -> Bool
intersects x = not . Set.null . Set.intersection x

maximum :: (Monoid (Maximum a), Foldable f) => f a -> a
maximum = getMaximum . concatMap Maximum . toList

maximum1 :: (Semigroup (Maximum a), Foldable f) => f a -> a
maximum1 = getMaximum . foldl1 (<>) . map Maximum . toList

invert :: (Semigroup (Maximum a), Foldable f, Functor f, Num a) => f a -> f a
invert xs = map (\x -> maximum1 xs - x) xs


-- | Like 'filter', but also supplying all 
-- other elements before or after in list to the predicate.
screen :: ([a] -> a -> [a] -> Bool) -> [a] -> [a]
screen p = go []
  where
  go xs [] = []
  go xs (y:ys) 
    | p xs y ys = y : go (xs ++ [y]) ys
    | otherwise = go xs ys
    
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

evalWriterT :: Monad m => WriterT w m a -> m a
evalWriterT = liftM fst . runWriterT

-- TODO replace tracE with printf
{-# INLINE tracE #-}
tracE :: [(String, String)] -> a -> a
tracE _ | not __trace__ = id
tracE [] = id
tracE ((n, s):xs) = id
  . Debug.trace ("\n\n[" ++ n ++ "]\n" ++ s) 
  . tracE xs

{-# INLINE trace #-}
trace :: String -> a -> a
trace | __trace__ = Debug.trace
      | otherwise  = \_ -> id

findIndicesM :: Monad m => (a -> m Bool) -> [a] -> m [Nat]
findIndicesM p = id
  . liftM concat 
  . zipWithM find [0..]
  where
  find i x = do
    is <- p x
    if is 
    then return [i]
    else return []

{-# INLINE error #-}
error :: String -> a
error = errorWithStackTrace
 
{-# INLINE errorf #-}
errorf :: PrintfArg a => String -> a -> b
errorf fmsg a = error (printf fmsg a)

-- | For default/informationless instances of things
class Empty a where
  empty :: a

instance Empty (Map k v) where
  empty = Map.empty

instance Empty (Set a) where
  empty = Set.empty

instance Empty [a] where
  empty = []

class MonadTrans t => Runnable t where
  runM :: Monad m => t m a -> m a

run :: Runnable t => t Identity a -> a
run = runIdentity . runM

instance Monoid w => Runnable (ReaderT w) where
  runM = ($ mempty) . runReaderT

readf :: (Read a, PrintfArg r) => String -> r -> a
readf str args = read (printf str args)
