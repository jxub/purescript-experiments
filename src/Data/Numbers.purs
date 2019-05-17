module Data.Numbers where

import Prelude

import Data.List (List(..))
import Data.Array (cons, head, tail, catMaybes)
import Data.Maybe (Maybe(..))

isEven :: Int -> Boolean
isEven num = case num of
    0         -> true
    1         -> false
    otherwise -> isEven (num - 2)

isEven2 :: Int -> Maybe Boolean
isEven2 num = Just (isEven num)

evenInts :: Array Int -> Int
evenInts arr = evenInts' (Just arr) 0

evenInts' :: Maybe (Array Int) -> Int -> Int
evenInts' arr ints = case arr of
    Just arr -> case head arr of
        Just x -> case isEven(x) of
            true  -> evenInts' (tail arr) (ints + 1)
            false -> evenInts' (tail arr) ints
        Nothing -> ints
    Nothing  -> ints

evenInts2 :: Array (Maybe Int) -> Int
evenInts2 arr = evenInts (catMaybes arr)

-- junk incoming

evenInts3 :: Array (Maybe Int) -> Int -> Maybe Boolean
evenInts3 arr ints = head arr >>= evenInt2

evenInt2 :: Maybe Int -> Maybe Boolean
evenInt2 n = n >>= isEven2