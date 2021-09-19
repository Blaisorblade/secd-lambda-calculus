module Prim where

data UnOp = Dec
  deriving (Show, Eq)

evalUnOp :: UnOp -> Int -> Int
evalUnOp Dec x = x - 1

data BinOp = Add | Mul
  deriving (Show, Eq)

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Add = (+)
evalBinOp Mul = (*)
