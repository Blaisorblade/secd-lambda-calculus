{-# LANGUAGE StrictData #-}
module Interp where

{- Quick implementation of CBV evaluation for lambda calculus + some primitives + fix. -}

import Prim
import Syntax

data Val
  = Closure [Val] Term
  | IntVal Int
  deriving Eq

-- Avoid infinite loops when showing infinite closures with a custom Show instance.

-- instance Show Val where
--   show (IntVal i) = "IntVal " ++ show i
--   show (Closure env t) = "Closure <env> " ++ show t

app_prec :: Int
app_prec = 10

instance Show Val where
  showsPrec d (IntVal i) =
    showParen (d > app_prec) $ showString "IntVal " . showsPrec (app_prec + 1) i
  showsPrec d (Closure _env t) =
    showParen (d > app_prec) $
    -- TODO: hacky attempt at showing part of the environment, minus the cycle. For debugging only.
    -- showString "Closure [<envHead>] " . showsPrec app_prec (tail (tail env)) . showString " " .
    showString "Closure <env> " .
    showsPrec (app_prec + 1) t

type Env = [Val]

eval :: Env -> Term -> Val
eval env = go where
  go (Var n) = env !! n
  go (Abs t) = Closure env t
  go (Fix t) =
    let Closure tEnv tBody = go t
        res = Closure (res : tEnv) tBody
    in res
  go (App ft at) =
    let av = go at
        Closure fEnv fBody = go ft
    in eval (av : fEnv) fBody

  go (IntLit pv) = IntVal pv
  go (Un op t) =
    let IntVal v = go t
    in IntVal $ evalUnOp op v
  go (Bin op t1 t2) =
    let IntVal v1 = go t1
        IntVal v2 = go t2
    in IntVal $ evalBinOp op v1 v2
  go (If0 c t e) =
    let IntVal cv = go c in
    if cv == 0 then
      go t
    else
      go e

testFact :: [Val]
testFact =
  eval [] . App fact . IntLit <$> [0..15]
