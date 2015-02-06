module Calc where

import Parser
import ExprT

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

test1 = eval (lit 3) == 3 &&
        eval (Add (lit 3) (lit 4)) == 7 &&
        eval (Mul (lit 7) (lit 2)) == 14 &&
        eval (Mul (Lit 2) (Add (Lit 3) (Lit 5))) == 16

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just x  -> Just $ eval x
    Nothing -> Nothing

test2 = evalStr "3" == Just 3 &&
        evalStr "2+3" == Just 5 &&
        evalStr "2*3" == Just 6 &&
        evalStr "(3*2)+4" == Just 10 &&
        evalStr "3*(2+4)" == Just 18

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

test3 = lit 3 == Lit 3 &&
        add (lit 3) (lit 4) == Add (Lit 3) (Lit 4) &&
        mul (lit 2) (lit 5) == Mul (Lit 2) (Lit 5) &&
        mul (lit 2) (add (lit 3) (lit 7)) == Mul (Lit 2) (Add (Lit 3) (Lit 7))

-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

