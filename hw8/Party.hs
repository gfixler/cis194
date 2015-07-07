module Party where

import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Data.Tree (Tree(Node))
import Employee


-- Exercise 1.1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL es fs) = GL (e:es) (fs+f)

-- Exercise 1.2
instance Monoid GuestList where
    mempty                         = GL [] 0
    (GL xs f) `mappend` (GL ys f') = GL (xs ++ ys) (f + f')

-- Exercise 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ x) r@(GL _ y) | x > y     = l
                              | otherwise = r


-- Exercise 2 helper function
treeTrav :: Tree a -> [a]
treeTrav (Node x []) = [x]
treeTrav (Node x xs) = x : concatMap treeTrav xs

-- Exercise 2
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc = foldr f acc . treeTrav

