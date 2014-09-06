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
    deriving (Show, Eq)

treeInsert :: a -> Tree a -> Tree a
treeInsert v Leaf = Node 0 Leaf v Leaf
treeInsert v (Node h Leaf v' Leaf) = Node (h+1) (treeInsert v Leaf) v' Leaf
treeInsert v (Node h t@(Node _ _ _ _) v' Leaf) = Node h t v' (treeInsert v Leaf)
treeInsert v (Node h Leaf v' t@(Node _ _ _ _)) = Node h (treeInsert v Leaf) v' t
treeInsert v (Node h l@(Node hl _ _ _) v' r@(Node hr _ _ _))
    | hl < hr   = Node (h+1) (treeInsert v l) v' r
    | otherwise = Node (h+1) l v' (treeInsert v r)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

