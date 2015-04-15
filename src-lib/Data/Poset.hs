{-
 - Copyright (C) 2009-2010 Nick Bowler.
 -
 - License BSD2:  2-clause BSD license.  See LICENSE for full terms.
 - This is free software: you are free to change and redistribute it.
 - There is NO WARRANTY, to the extent permitted by law.
 -}
 
{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}
module Data.Poset where

import qualified Data.List as List
import qualified Prelude
import Prelude hiding (Ordering(..), Ord(..))

import Data.Monoid

data Ordering = LT | EQ | GT | NC
    deriving (Eq, Show, Read, Bounded, Enum)

-- Lexicographic ordering.
instance Monoid Ordering where
    mempty = EQ
    mappend EQ x = x
    mappend NC _ = NC
    mappend LT _ = LT
    mappend GT _ = GT

-- | Internal-use function to convert our Ordering to the ordinary one.
toTotal :: Ordering -> Prelude.Ordering
toTotal LT = Prelude.LT
toTotal EQ = Prelude.EQ
toTotal GT = Prelude.GT
toTotal NC = error "Uncomparable elements in total order."

-- | Internal-use function to convert the ordinary Ordering to ours.
fromTotal :: Prelude.Ordering -> Ordering
fromTotal Prelude.LT = LT
fromTotal Prelude.EQ = EQ
fromTotal Prelude.GT = GT

-- | Class for partially ordered data types.  Instances should satisfy the
-- following laws for all values a, b and c:
--
-- * @a <= a@.
--
-- * @a <= b@ and @b <= a@ implies @a == b@.
--
-- * @a <= b@ and @b <= c@ implies @a <= c@.
--
-- But note that the floating point instances don't satisfy the first rule.
--
-- Minimal complete definition: 'compare' or '<='.
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    -- | Is comparable to.
    (<==>)  :: a -> a -> Bool
    -- | Is not comparable to.
    (</=>)  :: a -> a -> Bool
    (<)     :: a -> a -> Bool
    (<=)    :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    (>)     :: a -> a -> Bool

    a `compare` b
        | a == b = EQ
        | a <= b = LT
        | b <= a = GT
        | otherwise = NC

    a <    b = a `compare` b == LT
    a >    b = a `compare` b == GT
    a <==> b = a `compare` b /= NC
    a </=> b = a `compare` b == NC
    a <=   b = a < b || a `compare` b == EQ
    a >=   b = a > b || a `compare` b == EQ
