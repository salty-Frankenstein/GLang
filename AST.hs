{-# LANGUAGE GADTs #-}

module AST where

import Control.Monad.State
import Data.Char

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
  | ValNil
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  | Null Expr
  | Lambda (Name, Expr)
  | Apply Expr Expr
  | Let (Name, Expr) Expr
  | If Expr Expr Expr
  -- stmts
  | Assign (Name, Expr)
  | PutChar Expr
  | Ignore Expr
  | Do [Expr] -- do block
  deriving (Show)

  
{- interpret state monad -}
type InterpretM = StateT Environment IO

interpret :: Expr -> InterpretM ExprT
interpret (Assign (name, expr)) = do
  val <- interpret expr    -- eager eval
  modify $ envExt (name, val)
  return SNil
interpret (PutChar e) = do
  env <- get 
  SInt c <- interpret e 
  lift $ putChar (chr c)
  return SNil
interpret (Do b) = do
  oldEnv <- get
  res <- mapM interpret b
  -- get >>= lift.print
  put oldEnv
  return $ last res

interpret (Ignore expr) = undefined 

interpret (ValName n) = do
  env <- get
  case lookup n env of
    Just x -> return x
    Nothing -> error $ "undefined variable " ++ show n
    
interpret (ValInt i) = return $ SInt i
interpret (ValBool b) = return $ SBool b
interpret ValNil = return SNil

interpret l@Lambda {} = do
  env <- get
  return $ SFunc (l, env)
  
interpret (Let (n, e1) e2) = do
  env <- get 
  v1 <- interpret e1 
  put $ envExt (n, v1) env
  interpret e2
  
interpret (Apply f x) = do
  env <- get
  func <- interpret' f env
  case func of
    SFunc (Lambda (name, e), envSave) -> do
      v2 <- interpret' x env
      interpret' e (envExt (name, v2) envSave)
    _ -> error "this expression is not callable"
  where
    interpret' e env = do
      oldEnv <- get
      put env
      res <- interpret e
      put oldEnv
      return res
      
interpret (If cond expr1 expr2) = do
  SBool b <- interpret cond
  if b then interpret expr1 else interpret expr2 

interpret (Add e1 e2) = do
  SInt i1 <- interpret e1
  SInt i2 <- interpret e2
  return $ SInt (i1 + i2)
  
interpret (Sub e1 e2) = do
  SInt i1 <- interpret e1
  SInt i2 <- interpret e2
  return $ SInt (i1 - i2)
  
interpret (Mul e1 e2) = do
  SInt i1 <- interpret e1
  SInt i2 <- interpret e2
  return $ SInt (i1 * i2)
  
interpret (Div e1 e2) = do
  SInt i1 <- interpret e1
  SInt i2 <- interpret e2
  return $ SInt (i1 `div` i2)
  
interpret (Eq e1 e2) = do
  SInt i1 <- interpret e1
  SInt i2 <- interpret e2
  return $ SBool (i1 == i2)
  
interpret (Null e) = do
  res <- interpret e
  case res of
    SNil -> return $ SBool True 
    _ -> return $ SBool False

runInterpreter :: Expr -> IO ExprT
runInterpreter e = evalStateT (interpret e) env0
