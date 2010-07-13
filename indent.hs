{-# Language OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment.UTF8 (getArgs)
import System.IO.UTF8 (readFile)
import Text.ParserCombinators.Parsec

import GHC.Exts (IsString(..))

import Text.Syntactical
import qualified Text.Syntactical as S
import Text.Syntactical.String
import Language.Mojito.Syntax.Indent (Tree(..), strides)


----------------------------------------------------------------------
-- The token type for Syntactical
----------------------------------------------------------------------

data MyToken = MyToken Source String

data Source = Source Int Int -- source file line and column
            | Internal       -- generated

instance IsString MyToken where
  fromString = MyToken Internal

instance Token MyToken where
  toString (MyToken _ t) = t
  operator pt = Atom . MyToken Internal . concat . map toString $
    previousPart pt ++ [partSymbol pt]
  consider (MyToken _ a) (MyToken _ b) = a == b

----------------------------------------------------------------------
-- The operator table for Syntactical
----------------------------------------------------------------------

table0 :: Table MyToken
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

----------------------------------------------------------------------
-- Tokenizing, using a simple indentation scheme (provided by the
-- Indent module).
----------------------------------------------------------------------

type P a = GenParser Char () a

source :: P Source
source = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return $ Source l c

keywords :: [String]
keywords = words "let where of"

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for ,()⟨⟩[], which are each a single symbol.
sym :: P (Tree MyToken)
sym = try $ do
  src <- source
  x <- noneOf "\t\n "
  if x `elem` ",()⟨⟩[]"
    then spaces >> return (Sym $ MyToken src [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf ",()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords
        then pzero
        else spaces >> return (Sym . MyToken src $ x:xs)

-- Parse the empty-list symbol.
empty :: P (Tree MyToken)
empty = try $ do
  src <- source
  _ <- char '['
  spaces
  _ <- char ']'
  spaces
  return . Sym $ MyToken src "[]"

-- Parse a string literal.
str :: P (Tree MyToken)
str = try $ do
  src <- source
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return . Sym $ MyToken src ('"' : x ++ "\"")

tokenize = strides (empty <|> str <|> sym)
  p
  "{" "}" ";"
  where
  p = do
    src <- source
    str <- choice (map string $ words "let where of")
    return $ MyToken src str

----------------------------------------------------------------------
-- Simple command-line program
-- -i   just show the result of the tokenizing (indentation)
-- -fi  idem on a file
-- -f   apply the shunting yard to the file
--      apply the shunting yard to the argument
----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-i", s] -> case tokenize s of
      Right a -> putStrLn . unwords $ map toString a
      Left err -> putStrLn $ "indentation error: " ++ show err
    ["-fi", fn] -> do
      s <- readFile fn
      case tokenize s of
        Right a -> putStrLn . unwords $ map toString a
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

