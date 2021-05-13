module GLi where

import Parser
import AST

repl :: IO ()
repl = do
  putStr "彁> "
  -- code <- getLine
  code <- getCode ""
  if code == ":q" 
  then return ()
  else do
    let res = parse code
    case res of
      Right expr -> print $ eval expr env0
      err -> print err
    repl

main :: IO ()
main = do
  putStrLn "彁Lang interpreter, version 0._"
  repl