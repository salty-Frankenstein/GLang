{-# LANGUAGE GADTs #-}

module AST where

import Control.Monad.State

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
  | SNil

instance Show ExprT where
  show (SInt i) = show i
  show (SBool b) = if b then "#t" else "#f"
  show (SFunc _) = "<function>"
  show SNil = "nil"

data Expr
  = ValInt Int
  | ValBool Bool
  | ValName Name
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  | Lambda (Name, Expr)
  | Apply Expr Expr
  | Let (Name, Expr) Expr
  | If Expr Expr Expr
  -- stmts
  | Assign (Name, Expr)
  | Ignore Expr
  | Do [Expr] -- do block
  deriving (Show)

  
{- interpret state monad -}
type InterpretM = StateT Environment IO

interpret :: Expr -> InterpretM ExprT
interpret (Assign (name, expr)) = do
  env <- get 
  let val = eval expr env   -- eager eval
  put $ envExt (name, val) env
  return SNil
interpret (Do b) = do
  oldEnv <- get
  res <- mapM interpret b
  get >>= lift.print
  put oldEnv
  return $ last res

interpret (Ignore expr) = undefined 

interpret e = gets $ eval e

runInterpreter :: Expr -> IO ExprT
runInterpreter e = evalStateT (interpret e) env0

eval :: Expr -> Environment -> ExprT
eval e env =
  case e of
    ValName n ->
      case lookup n env of
        Just x -> x
        Nothing -> error $ "undefined variable " ++ show n
    ValInt i -> SInt i
    ValBool b -> SBool b
    l@Lambda {} -> SFunc (l, env)
    Let (n, e1) e2 ->
      let v1 = eval e1 env
       in eval e2 (envExt (n, v1) env)
    Apply f x ->
      case eval f env of
        SFunc (Lambda (name, e), envSave) ->
          let v2 = eval x env
           in eval e (envExt (name, v2) envSave)
        _ -> error "this expression is not callable"
    If cond expr1 expr2 ->
      let (SBool b) = eval cond env
       in if b then eval expr1 env else eval expr2 env
    Add e1 e2 ->
      let (SInt i1) = eval e1 env
          (SInt i2) = eval e2 env
       in SInt (i1 + i2)
    Sub e1 e2 ->
      let (SInt i1) = eval e1 env
          (SInt i2) = eval e2 env
       in SInt (i1 - i2)
    Mul e1 e2 ->
      let (SInt i1) = eval e1 env
          (SInt i2) = eval e2 env
       in SInt (i1 * i2)
    Div e1 e2 ->
      let (SInt i1) = eval e1 env
          (SInt i2) = eval e2 env
       in SInt (i1 `div` i2)
    Eq e1 e2 ->
      let (SInt i1) = eval e1 env
          (SInt i2) = eval e2 env
       in SBool (i1 == i2)
x = Do [
    Assign ("f", Lambda ("x", 
         ValInt 1 `Add`
         Apply (ValName "f") (ValName "x" `Sub` ValInt 1 ))
      )
    -- , ValName "f"
    ,Apply (ValName "f") (ValInt 2)
  ]

xx = Apply (ValName "x") (ValName "x")
fxx = Apply (ValName "f") xx
lxfxx = Lambda ("x", fxx)

y = Lambda ("f",
    Apply lxfxx lxfxx
  )

fact = Lambda ("f",
    Lambda ("n", 
      If (ValName "n" `Eq` ValInt 0) 
        (ValInt 1)
        (ValName "n" `Mul` 
          Apply (ValName "f") (ValName "n" `Sub` ValInt 1))
    )
  )
  
t = Do [
    Assign ("y", y),
    Apply (Apply (ValName "y") fact) (ValInt 4)
  ]