module GLang where

import Parser
import AST
import System.Environment

main :: IO ()
main = do
  args <- getArgs 
  case args of
    [path] -> do
      code <- readFile path
      case parse code of
        Right e -> do
          res <- runInterpreter e
          case res of
            SNil -> return ()
            r -> print r
        _ -> return ()
    _ -> error "argument error"
