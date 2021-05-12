{-# LANGUAGE GADTs #-}

module AST where

import Text.Parsec

type Name = String

type Environment = [(Name, ExprT)]

env0 :: Environment
env0 = []

envExt :: (Name, ExprT) -> Environment -> Environment
envExt = (:)

type FuncT = (Expr, Environment) -- body and env

-- all possible Expr types of my Scheme
data ExprT
  = SInt Int
  | SBool Bool
  | SName Name
  | SFunc FuncT

instance Show ExprT where
  show (SInt i) = show i
  show (SBool b) = if b then "#t" else "#f"
  show (SFunc _) = "<function>"

data Expr
  = ValInt Int
  | ValBool Bool
  | ValName Name
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lambda (Name, Expr)
  | Apply Expr Expr
  | Let (Name, Expr) Expr
  deriving Show

-- evalExpr :: Expr a -> a
-- evalExpr (ValInt i) = i
-- evalExpr (ValBool b) = b
-- evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
-- evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
-- evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
-- evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2

interpret :: Expr -> Environment -> ExprT
interpret e env =
  case e of
    ValName n ->
      case lookup n env of
        Just x -> x
        Nothing -> error $ "undefined variable" ++ show n
    ValInt i -> SInt i
    ValBool b -> SBool b
    l@Lambda {} -> SFunc (l, env)
    Let (n, e1) e2 ->
      let v1 = interpret e1 env
       in interpret e2 (envExt (n, v1) env)
    Apply f x ->
      case interpret f env of
        SFunc (Lambda (name, e), envSave) ->
          let v2 = interpret x env
           in interpret e (envExt (name, v2) envSave)
        _ -> error "this expression is not callable"
    Add e1 e2 ->
      let (SInt i1) = interpret e1 env
          (SInt i2) = interpret e2 env
       in SInt (i1 + i2)
    Sub e1 e2 ->
      let (SInt i1) = interpret e1 env
          (SInt i2) = interpret e2 env
       in SInt (i1 - i2)
    Mul e1 e2 ->
      let (SInt i1) = interpret e1 env
          (SInt i2) = interpret e2 env
       in SInt (i1 * i2)
    Div e1 e2 ->
      let (SInt i1) = interpret e1 env
          (SInt i2) = interpret e2 env
       in SInt (i1 `div` i2)
