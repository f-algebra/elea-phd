module Elea.Monad.Error.Assertion
  ( Assert, bool, equal, check, checkM, augment,
    success, failure, isSuccess, firstFailure, toError,
    assert, assertEq )
where

import Elea.Prelude
import qualified Elea.Monad.Error.Class as Err

type Assert = Err.Error ()

success :: Assert
success = return ()

failure :: String -> Assert
failure msg = Err.throw (read msg)

isSuccess :: Assert -> Bool
isSuccess = not . Err.wasThrown

fromFailure :: Assert -> Err.Stack
fromFailure (runEitherT -> Identity (Left stk)) = stk 

-- | Returns the first failure, if one exists.
firstFailure :: [Assert] -> Assert
firstFailure [] = success
firstFailure (x:xs) 
  | isSuccess x = firstFailure xs
  | otherwise = x

augment :: String -> Assert -> Assert
augment = Err.augment . read

{-# INLINE check #-}
check :: Assert -> a -> a
checkM :: Monad m => Assert -> m ()
#ifndef ASSERT
check _ = id
checkM _ = return ()
#else
check asrt 
  | isSuccess asrt = id
  | otherwise = error (show (fromFailure asrt))
checkM assert =
  check assert (if isSuccess assert then return () else fail "")
#endif

toError :: (Err.Throws m, Err.Err m ~ Err.Stack) => Assert -> m ()
toError assert 
  | isSuccess assert = return ()
  | otherwise = Err.throw (fromFailure assert)

bool :: Bool -> Assert
bool b = if b then success else failure ""

equal :: (Show a, Eq a) => a -> a -> Assert
equal x y 
  | x == y = success
  | otherwise = failure (printf "expected %s but got %s" (show x) (show y))

assert :: String -> Bool -> a -> a
assert msg = check . augment msg . bool 

assertEq :: (Show a, Eq a) => String -> a -> a -> b -> b
assertEq msg x y = check . augment msg $ equal x y 

