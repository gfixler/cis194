module Party where

import Data.List (sort)
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


-- Exercise 2
treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f l (Node x ts) = f x $ map (treeFold f l) ts


-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (glCons e wo, w)
    where w  = mconcat $ map fst gs
          wo = mconcat $ map snd gs


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t = let (x,y) = treeFold nextLevel [] t in max x y


printGuestList :: GuestList -> IO ()
printGuestList (GL es f) = do
    putStrLn $ "Total fun: " ++ show f
    let es' = sort $ map empName es
    mapM_ print es'

main = do
    emps <- readFile "company.txt"
    let emps' = read emps :: Tree Employee
    printGuestList $ maxFun emps'

