module Main where

import Prelude hiding (readFile)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment.UTF8 (getArgs)
import System.IO.UTF8 (readFile)
import Text.ParserCombinators.Parsec

import Text.Syntactical
import Text.Syntactical.String
import Language.Mojito.Syntax.Indent (Tree(..), strides)

table0 :: Table String
table0 = buildTable
 [ [ closed "(" Distfix ")"
   , closed "⟨" SExpression "⟩"
   , closed "[" Distfix "]"
   , closed "[" Distfix "|" `distfix` "]"
   , closed "{" Distfix "}"
   ]
 , [ infx RightAssociative "?'" `distfix` ":'"
   , postfx "!"
   , postfx "_/" `distfix` "/."
   ]
 , [ postfx "%"
   , prefx "#"
   ]
 , [ postfx "°"
   , infx LeftAssociative "*"
   , infx LeftAssociative "/"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "<<"
   , infx LeftAssociative ">>"
   , infx RightAssociative "?" `distfix` ":"
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative "->"
   ]
 , [ infx RightAssociative ","
   ]
 , [ prefx "let" `distfix` "in"
   , infx RightAssociative "where"
   , prefx "case" `distfix` "of"
   ]
 , [ prefx "\\" `distfix` "->"
   , prefx "λ" `sexpr` "."
   ]
 , [ infx RightAssociative "::"
   , infx RightAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 ]

type P a = GenParser Char () a

keywords :: [String]
keywords = words "let where of"

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for ,()⟨⟩[], which are each a single symbol.
sym :: P Tree
sym = try $ do
  x <- noneOf "\t\n "
  if x `elem` ",()⟨⟩[]"
    then spaces >> return (Sym [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf ",()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords then pzero else spaces >> return (Sym $ x:xs)

-- Parse the empty-list symbol.
empty :: P Tree
empty = try $ do
  _ <- char '['
  spaces
  _ <- char ']'
  spaces
  return $ Sym "[]"

-- Parse a string literal.
str :: P Tree
str = try $ do
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return $ Sym ('"' : x ++ "\"")

tokenize = strides (empty <|> str <|> sym) (words "let where of") "{" "}" ";"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-i", s] -> case tokenize s of
      Right a -> putStrLn $ unwords a
      Left err -> putStrLn $ "indentation error: " ++ show err
    ["-fi", fn] -> do
      s <- readFile fn
      case tokenize s of
        Right a -> putStrLn $ unwords a
        Left err -> putStrLn $ "indentation error: " ++ show err
    ["-f", fn] -> do
      s <- readFile fn
      case tokenize s of
        Right a -> case shunt table0 . map Atom $ a of
          Right e -> putStrLn $ showSExpr e
          Left f -> putStrLn $ showFailure f
        Left err -> putStrLn $ "indentation error: " ++ show err
    [s] -> case tokenize s of
      Right a -> case shunt table0 . map Atom $ a of
        Right e -> putStrLn $ showSExpr e
        Left f -> putStrLn $ showFailure f
      Left err -> putStrLn $ "indentation error: " ++ show err
    _ -> putStrLn "Usage: (TODO)"

