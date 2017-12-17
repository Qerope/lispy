module Main where

import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print (readExpr (head args))
