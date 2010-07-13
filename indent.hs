module Main where

import Prelude hiding (readFile)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment.UTF8 (getArgs)
import System.IO.UTF8 (readFile)

import Text.Syntactical
import Text.Syntactical.String
import Language.Mojito.Syntax.Indent (strides)

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

tokenize = strides (words "let where of") "{" "}" ";"

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

