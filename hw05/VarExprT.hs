module VarExprT where

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
    deriving (Show, Eq)

