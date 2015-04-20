{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Data.Maybe
import Sized (Size(..), Sized, size, getSize)
import Scrabble (Score, score, scoreString)
import Buffer (Buffer(..))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

-- Exercise 2, part 1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ i _              | i < 0 = Nothing
indexJ i (Single _ a)   = if i == 0 then Just a else Nothing
indexJ i (Append _ l r) = if i < i' then indexJ i l
                                    else indexJ (i-i') r
    where i' = getSize . size $ tag l

-- Exercise 2, part 2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty          = Empty
dropJ n j              | n <= 0 = j
dropJ n s@(Single _ _) = if n == 0 then s else Empty
dropJ n (Append _ l r) | n < n'  = dropJ n l +++ r
                       | n > n'  = dropJ (n-n') r
                       | n == n' = r
    where n' = getSize . size $ tag l

-- Exercise 2, part 3

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ n j              | n < 0 = Empty
takeJ n s@(Single _ _) = if n == 0 then Empty else s
takeJ n (Append _ l r) | n < n'  = takeJ n l
                       | n > n'  = l +++ takeJ (n-n') r
                       | n == n' = l
    where n' = getSize . size $ tag l

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

singleLine :: String -> JoinList (Score, Size) String
singleLine s = Single (scoreString s, Size $ length s) s

reduce :: [JoinList (Score, Size) String] -> [JoinList (Score, Size) String]
reduce [] = []
reduce [x] = [x]
reduce (x:y:ys) = x +++ y : reduce ys

reduceAll :: [JoinList (Score, Size) String] -> JoinList (Score, Size) String
reduceAll xs = if xs == reduce xs then if xs == [] then Empty else head xs
                                                   else reduceAll $ reduce xs

instance Buffer (JoinList (Score, Size) String) where
    toString b = undefined
    fromString = undefined
    line i s = undefined
    replaceLine i s b = undefined
    numLines b = undefined
    value b = undefined

