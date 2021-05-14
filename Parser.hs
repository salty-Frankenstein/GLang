module Parser where

import AST
import Data.Functor
import Text.Parsec

keyword = "彁恷垉垈墸壥汢熕粭挧暃椦橸碵粐糘"

isKeyword, notKeyword :: Parsec String () Char
isKeyword = oneOf keyword
notKeyword = noneOf keyword

parseNil :: Parsec String () Expr
parseNil = try (char '碵') >> return ValNil 

parseBool :: Parsec String () Bool
parseBool = (try (char '粐') >> return True)
        <|> (try (char '糘') >> return False)

parseNum :: Parsec String () Int
parseNum = many1 (char '岾') <&> length

parseName :: Parsec String () Name
parseName = try (many1 notKeyword)

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
            <|> (char '暃' >> return Eq)

parseOp1 = do
  op <- parseOp1'
  expr <- parseTerm
  return $ op expr
  where
    parseOp1' = (char '橸' >> return Null)

parseLambda :: Parsec String () Expr
parseLambda = do
  try (char '汢')
  name <- parseName
  expr <- parseTerm
  return $ Lambda (name, expr)

parseApply :: Parsec String () Expr
parseApply = do
  func <- parseTerm
  expr <- parseTerm
  return $ Apply func expr


parseIf = do
  try (char '挧')
  cond <- parseTerm
  expr1 <- parseTerm
  expr2 <- parseTerm
  return $ If cond expr1 expr2

parseBlock :: Parsec String () Expr
parseBlock = do
  try (char '熕')
  exprs <- many parseTerm
  return $ Do exprs

parseAssign :: Parsec String () Expr
parseAssign = do
  try (char '粭')
  name <- parseName
  expr <- parseTerm
  return $ Assign (name, expr)

parsePutChar = do
  try (char '椦')
  expr <- parseTerm
  return $ PutChar expr

parseExpr :: Parsec String () Expr
parseExpr = (parseNum <&> ValInt)
        <|> (parseBool <&> ValBool)
        <|> parseNil
        <|> (parseName <&> ValName)
        <|> parseOp1
        <|> parseOp2
        <|> parseLambda
        <|> parseApply
        <|> parseBlock
        <|> parseAssign
        <|> parseIf
        <|> parsePutChar

parseTerm = between (char '彁') (char '恷') parseExpr 

whiteSpace = " \t\n"

parse :: String -> Either ParseError Expr
parse code = runP parseTerm () "n" (filter (`notElem` whiteSpace) code)   

getCode :: String -> IO String
getCode preCode = do
  new <- getLine 
  let code = filter (`notElem` whiteSpace) (preCode ++ new)
  putStrLn $ "<" ++ code ++ ">"
  case runP check () "n" code of
    Right expr -> return code
    x -> do
      print x
      getCode code
  
-- FIXME
check :: Parsec String () ()
check = ((skipMany (noneOf "彁恷")) 
  >> (between (char '彁') (char '恷') check )
  >> (skipMany (noneOf "彁恷")))
  <|> (skipMany (noneOf "彁恷"))