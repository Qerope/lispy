module Main where

import Parser
import Eval
import Types
import System.Environment

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

main :: IO ()
main = do
  args <- getArgs
  evalAndPrint (head args)
