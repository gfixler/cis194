module Calc where

import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    add, mul :: a -> a -> a

instance Expr ExprT where
    lit a = Lit a
    add a b = Add a b
    mul a b = Mul a b

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    (Just expr) -> Just $ eval expr
    otherwise   -> Nothing

