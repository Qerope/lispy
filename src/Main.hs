module Main where

import Parser
import Eval
import Types
import System.IO
import System.Environment

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "lispy> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [expr] -> evalAndPrint expr
    _ -> putStrLn "usage: lispy [expression]"
