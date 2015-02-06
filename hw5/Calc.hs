module Calc where

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

