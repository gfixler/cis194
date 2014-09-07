fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (`subtract` 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate hail
    where hail = \n -> if odd n then 3 * n + 1 else n `div` 2


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Ord, Show, Eq)

sprout :: a -> Tree a
sprout v = Node 0 Leaf v Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert v Leaf = sprout v
treeInsert v (Node h Leaf tv Leaf) = Node (h+1) (sprout v) tv Leaf
treeInsert v (Node h Leaf tv t) = Node (h+1) (sprout v) tv t
treeInsert v (Node h t tv Leaf) = Node (h+1) t tv (sprout v)
treeInsert v (Node h l@(Node hl _ _ _) tv r@(Node hr _ _ _))
    | hl < hr   = Node (h+1) (treeInsert v l) tv r
    | otherwise = Node (h+1) l tv (treeInsert v r)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf


xor :: [Bool] -> Bool
xor = foldl (\a b -> if b then a /= b else a) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

