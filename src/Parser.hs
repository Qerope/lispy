module Parser (readExpr) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (escaped <|> noneOf "\"\\")
  _ <- char '"'
  return $ String x
  where
    escaped = char '\\' >> choice
      [ char 'n' >> return '\n', char 't' >> return '\t'
      , char '"' >> return '"', char '\\' >> return '\\' ]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  sign <- option "" (string "-")
  digits <- many1 digit
  return $ Number (read (sign ++ digits))

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseNumber <|> parseAtom <|> parseString <|> parseQuoted
  <|> do _ <- char '('
         x <- try parseList <|> parseDottedList
         _ <- char ')'
         return x
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow p input = case parse p "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
