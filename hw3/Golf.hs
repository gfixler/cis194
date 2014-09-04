module Golf where

nths :: Int -> [a] -> [a]
nths n xs
    | length xs < n = []
nths n xs = head (drop (n-1) xs) : nths n (drop n xs)

skips :: [a] -> [[a]]
skips xs = [nths x xs | x <- [1..(length xs)]]

