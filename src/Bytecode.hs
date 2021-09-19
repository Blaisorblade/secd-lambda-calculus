{-# LANGUAGE StrictData #-}
module Bytecode where

import Prim
-- import qualified Interp as I
--import Interp(BinOp(..))
import Interp(app_prec)

data OpCode
  = Var Int
  | MkClosure [OpCode]
  | Fix
  | App

  | IntLit Int
  | Un UnOp
  | Bin BinOp

  | If0 [OpCode] [OpCode]

  | Ret
  deriving Show

type Code = [OpCode]

data Val
  = Closure [Val] Code
  | IntVal Int
instance Show Val where
  showsPrec d (IntVal i) =
    showParen (d > app_prec) $ showString "IntVal " . showsPrec (app_prec + 1) i
  showsPrec d (Closure _env t) =
    showParen (d > app_prec) $
    -- TODO: hacky attempt at showing part of the environment, minus the cycle. For debugging only.
    -- showString "Closure [<envHead>] " . showsPrec app_prec (tail (tail env)) . showString " " .
    showString "Closure <env> " .
    showsPrec (app_prec + 1) t


type Stack = [Val]
type Env = [Val]

eval :: Env -> Code -> Stack -> (Val, Stack)
eval env = go where
  go :: Code -> Stack -> (Val, Stack)
  go (Ret : _rest) (v : _stack) = (v, [])
  go (op : rest) stack =
    let (v, stack') = goOp op stack in
    go rest (v : stack')
  go [] _ = error "Empty code"

  goOp :: OpCode -> Stack -> (Val, Stack)
  goOp (Var n) stack = (env !! n, stack)
  goOp (MkClosure c) stack =
    (Closure env c, stack)
  goOp Fix (Closure tEnv tBody : stack) =
    let res = Closure (res : tEnv) tBody
    in (res, stack)
  goOp App (Closure fEnv fBody : av : stack) =
    let (v, _calleeStack) = eval (av : fEnv) fBody [] in
    (v, stack)

  goOp (IntLit n) stack = (IntVal n, stack)
  goOp (Un op) (IntVal v : stack) =
    (IntVal $ evalUnOp op v, stack)
  goOp (Bin op) (IntVal v1 : IntVal v2 : stack)=
    (IntVal $ evalBinOp op v1 v2, stack)
  goOp (If0 t e) (IntVal cv : stack) =
    if cv == 0 then
      eval env t stack
    else
      eval env e stack
  goOp _code _stack = undefined
