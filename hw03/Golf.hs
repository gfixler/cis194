module Golf where

import Data.List
import Data.Maybe


-- nths is a recursive function that uses a pattern to exit when there aren't
-- enough elements remaining in the list to provide a next element at n places
-- ahead of the previous one, and otherwise consing the head n elements into
-- the remaining list (obtained by dropping everything before it via n-1) with
-- the rest of the list, which is fed through the same process
nths :: Int -> [a] -> [a]
nths n xs
    | length xs < n = []
nths n xs = head (drop (n-1) xs) : nths n (drop n xs)

-- Based on the axiom that for any list of length n, skips will produce an
-- n-length list of lists, I used a list-comprehension over the range 1-n,
-- calling the nths helper function with that value on the input list to
-- generate each skip list of the original.
skips :: [a] -> [[a]]
skips xs = [nths x xs | x <- [1..(length xs)]]


-- maxima returns the middle Integer in a given 3-tuple, wrapped in a Maybe,
-- iff that value is smaller than the first and last values in the triple, else
-- it returns a Nothing
maxima :: (Integer,Integer,Integer) -> Maybe Integer
maxima (a,b,c)
    | a < b && c < b = Just b
    | otherwise      = Nothing

-- localMaxima takes its input list and offsets it twice, zipping the three
-- lists together to create triples that put each value capable of having a
-- local maxima (i.e. not the one on each end of the list) between its
-- neighbors, setting things up for mapping with maxima, which is done with
-- mapMaybe, as maxima returns a Maybe value
localMaxima :: [Integer] -> [Integer]
localMaxima xs = mapMaybe maxima (zip3 xs (tail xs) (tail $ tail xs))


-- row takes a list of Integers - assumed 0-9, inclusive - and uses a list
-- comprehension to build a String from the numbers 0-9, inclusive, using the
-- fact that a list of Chars in Haskell is a string - with a space for each
-- digit not appearing in the list, and an asterisk for each that does, which
-- effectively creates a height-1 histogram indicating element presence
row :: [Integer] -> String
row r = [if elem i r then '*' else ' ' | i <- [0..9]]

-- wipe uses map to create a list of partially-applied deletion functions over
-- the range 0-9, which will each delete a single instance of their digit from
-- a list, and this list is then folded via lambda expression over the input
-- list, wiping out one instance of each digit from 0-9, inclusive, if present
wipe :: [Integer] -> [Integer]
wipe xs = foldl (\a b -> b a) xs (map (delete) [0..9])

-- bars is a recursive function that keeps creating single-character-high
-- histograms for the digits 0-9, inclusive - which remain from the given input
-- list; when the the current row is created via the row function, the
-- remainder has another copy of the digits removed via the wipe function, and
-- then goes through bars again to create another histogram of the remainder
bars :: [Integer] -> [String]
bars [] = []
bars xs = row xs : bars (wipe xs)

-- histogram pulls all of the cross-sections of the histogram for the given
-- list from the bars function, concatenates them with the histogram label and
-- ASCII art separator, all of which is then reversed and unlines'd together to
-- create the final String, which is the histogram of the given 0-9 digits
histogram :: [Integer] -> String
histogram xs = unlines . reverse $ [['0'..'9'],"=========="] ++ bars xs

