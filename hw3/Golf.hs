module Golf where

import Data.List
import Data.Maybe


nths :: Int -> [a] -> [a]
nths n xs
    | length xs < n = []
nths n xs = head (drop (n-1) xs) : nths n (drop n xs)

skips :: [a] -> [[a]]
skips xs = [nths x xs | x <- [1..(length xs)]]


maxima :: (Integer,Integer,Integer) -> Maybe Integer
maxima (a,b,c)
    | a < b && c < b = Just b
    | otherwise      = Nothing

localMaxima :: [Integer] -> [Integer]
localMaxima xs = catMaybes $ map maxima (zip3 xs (tail xs) (tail $ tail xs))

row :: [Integer] -> String
row r = [if elem i r then '*' else ' ' | i <- [0..9]]

wipe :: [Integer] -> [Integer]
wipe xs = foldl (\a b -> b a) xs (map (delete) [0..9])

bars :: [Integer] -> [String]
bars [] = []
bars xs = row xs : bars (wipe xs)

histogram :: [Integer] -> String
histogram xs = unlines . reverse $ [['0'..'9'],"=========="] ++ bars xs

