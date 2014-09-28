import ExprT
import Parser


eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr e = case parseExp Lit Add Mul e of
    Just s -> Just (eval s)
    _      -> Nothing

