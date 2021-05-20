module Main where

-- This program can act as a FastCGI executable when the
-- program name is simple.fcgi.

import System.Environment
import System.Exit (exitFailure)
import Text.PrettyPrint.HughesPJClass
import Test.HUnit

import Language.Mojito.Syntax.SExpr
import Language.Mojito.Syntax.ExprBuilder
import qualified Language.Mojito.Inference.Cardelli.Prelude as C
  (someEnvironment)
import qualified Language.Mojito.Inference.Cardelli.Cardelli as C
  (infer)
import Language.Mojito.Inference.SystemCT1999.Prelude
import Language.Mojito.Inference.SystemCT1999.SystemCT1999

main :: IO ()
main = do
  who <- getProgName
  if who == "simple.fcgi"
    then putStrLn "TODO: simple.fcgi"
    else normal

try :: String -> Either String a -> IO a
try msg e = case e of
  Left err -> putStrLn (msg ++ err) >> exitFailure
  Right a -> return a

normal :: IO ()
normal = do
  as <- getArgs
  case as of
    ["--help"] -> usage

    ["--run-tests"] -> tests

    ["--sexpr", s] -> do
      sexpr <- try "Parse error: " $ parseSExprs' s
      print sexpr

    ["--expr", s] -> do
      sexpr <- try "Parse error: " $ parseSExprs' s
      expr <- try "Wrong s-expression: " $ sexprsToExpr sexpr
      print expr

    ["--milner", s] -> do
      sexpr <- try "Parse error: " $ parseSExprs' s
      expr <- try "Wrong s-expression: " $ sexprsToExpr sexpr
      case C.infer expr C.someEnvironment of
        ((Left err,_),_) -> putStrLn $ "Type-checking failed: " ++ err
        ((Right typedExpr,_),_) -> print typedExpr

    ["--system-ct", s] -> do
      sexpr <- try "Parse error: " $ parseSExprs' s
      expr <- try "Wrong s-expression: " $ sexprsToExpr sexpr
      case infer someTypes expr someContext of
        ((Left err,_),_) -> putStrLn $ "Type-checking failed: " ++ err
        ((Right (c,g),_),inf) -> do
          typedExpr <- try "wrong final type: " $ duplicate' inf expr c g
          print $ pPrint typedExpr

    _ -> putStrLn "Unrecognized arguments." >> usage

usage :: IO ()
usage = do
  me <- getProgName
  putStr . unlines $
    concat [ "Usage: ", me, " [OPTION]"] :
    "Options:" :
    "  --help                       Print this message" :
    "  --run-tests                  Run the test suite" :
    "  --sexpr <string>             Parse the string as an s-expression" :
    "                               and print it" :
    "  --expr <string>              Parse the string as an abstract syntax" :
    "                               tree and print it." :
    "  --milner <string>            Parse the string as an abstract syntax" :
    "                               tree, infer the types, and p-print it." :
    "  --system-ct <string>         Parse the string as an abstract syntax" :
    "                               tree, infer the types, and p-print it." :
    []

tests :: IO ()
tests = putStrLn "TODO --run-tests"
