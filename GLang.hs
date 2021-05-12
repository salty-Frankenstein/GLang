module GLang where

import Parser
import Text.Parsec

main :: IO ()
main = do
  code <- getLine
  print code
  print $ runP parseTerm () "n" code  
  return ()