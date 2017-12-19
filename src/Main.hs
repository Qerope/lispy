module Main where

import Parser
import Eval
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print (eval (readExpr (head args)))
