module Parser where

import AST
import Data.Functor
import Text.Parsec

isKeyword, notKeyword :: Parsec String () Char
isKeyword = oneOf "彁恷垉垈墸壥汢"
notKeyword = noneOf "彁恷垉垈墸壥汢"

parseNum :: Parsec String () Int
parseNum = many1 (char '岾') <&> length

parseName :: Parsec String () Name
parseName = many1 notKeyword

parseOp2 :: Parsec String () Expr
parseOp2 = do
      op <- parseOp2'
      l <- parseTerm
      r <- parseTerm
      return $ op l r
  where 
    parseOp2' = (char '垉' >> return Add)
            <|> (char '垈' >> return Sub)
            <|> (char '墸' >> return Mul)
            <|> (char '壥' >> return Div)

parseLambda :: Parsec String () Expr
parseLambda = do
  char '汢'
  name <- parseName
  expr <- parseTerm
  return $ Lambda (name, expr)

parseApply :: Parsec String () Expr
parseApply = do
  func <- parseTerm
  expr <- parseTerm
  return $ Apply func expr

parseExpr :: Parsec String () Expr
parseExpr = (parseNum <&> ValInt)
        <|> (parseName <&> ValName)
        <|> parseOp2
        <|> parseLambda
        <|> parseApply

parseTerm = between (char '彁') (char '恷') parseExpr 

parse :: String -> Expr
parse = undefined
