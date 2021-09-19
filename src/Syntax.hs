{-# LANGUAGE StrictData #-}
module Syntax where

import Prim

data Term
  = Var Int
  | Abs Term
  | Fix Term
  | App Term Term

  | IntLit Int
  | Un UnOp Term
  | Bin BinOp Term Term

  | If0 {- cond -} Term {- then -} Term {- else -} Term
  deriving (Show, Eq)

-- Example term.
fact :: Term
fact = Fix (Abs factBody)
  where
    factParam = Var 1
    nParam = Var 0
    factBody =
      If0 nParam
        (IntLit 0) $
        If0 (Un Dec nParam)
          (IntLit 1)
          (Bin Mul nParam (App factParam (Un Dec nParam)))
