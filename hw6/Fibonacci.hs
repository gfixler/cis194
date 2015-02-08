{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

import Data.List

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = listWrap . intercalate "," . map show . take 20 . streamToList
        where listWrap = ("["++) . (++",...]")

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) =
    Cons a (Cons b (interleaveStreams as bs))

ruler :: Stream Integer
ruler = doRuler 0
    where doRuler n = Cons n (interleaveStreams (doRuler (n+1))
                                                (streamRepeat n))

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- suggested by pjdelport on #haskell-beginners
evalLim :: Stream Integer -> Int -> Integer -> Integer
evalLim cs l x = foldr (\(c,e) a -> a + c * x^e) 0 stream
    where stream = take l $ zip (streamToList cs) [0..]

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate = streamMap negate
    (Cons x xs) + (Cons y ys) = Cons (x + y) (xs + ys)
    (Cons x xs) * yss@(Cons y ys) = Cons (x * y)
                                         (streamMap (*x) ys + xs * yss)

