{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Calc where

import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a
        | a > 0     = True
        | otherwise = False
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Num, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Ord, Num, Show)

instance Expr MinMax where
    lit = MinMax
    add = min
    mul = max

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add = (+)
    mul = (*)

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    (Just expr) -> Just $ eval expr
    otherwise   -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

