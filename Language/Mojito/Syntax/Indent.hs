-- Simple indentation parser for Parsec. It constructs blocks of list of
-- continued lines (called strides). It is parametrized by the strings
-- that introduce indented blocks and by the parser for the leaves of the
-- tree.
module Language.Mojito.Syntax.Indent where

import Text.ParserCombinators.Parsec
import Control.Monad (unless)

-- A tree data structure to represent indentation.
data Tree =
    Sym String
  | Block String [Stride] -- Block s ss represents an indented block
                       -- introduced by the string s.
  deriving Show

newtype Stride = Stride [Tree]
  deriving Show

-- Flatten a list of strides (that is a block) into a list of
-- token, introducing indent, dedent, and sequence tokens.
flatten :: String -> String -> String -> [Stride] -> [String] -> [String]
flatten i d sq = symStrides
  where
    symStrides [s] = symStride s
    symStrides (s:ss) = symStride s . (sq :) . symStrides ss

    symStride (Stride ts) = symTrees ts

    symTrees [] = id
    symTrees (t:ts) = symTree t . symTrees ts

    symTree (Sym x) = (x :)
    symTree (Block name ss) =
      (\a -> name : i : a) .
      symStrides ss .
      (d :)

type Pos = (Int,Int)

type P a = GenParser Char () a

-- Return the current source line and source column.
getPos :: P Pos
getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

-- Continue parsing (using p) after position (l1,c1).
continue :: Pos -> P a -> P a
continue (l1,c1) p = do
  (l2,c2) <- getPos
  unless (c1 < c2 || l1 == l2) pzero
  p

-- aligned p parses many1 p with all p aligned on the same column.
aligned :: P a -> P [a]
aligned p = do
  -- get the defining column
  (_,dc) <- getPos
  -- many1 p but only with p starting exactly at dc
  many1 (getPos >>= \(_,c) -> unless (c == dc) pzero >> p)

-- Parse one of the given strings then a block that starts on the
-- same line or on the same or grater column.
indent :: P Tree -> [String] -> P Tree
indent atom intro = try $ do
  (l1,c1) <- getPos
  s <- choice (map string intro)
  spaces
  (l2,c2) <- getPos
  unless (c1 <= c2 || l1 == l2) pzero
  b <- block atom intro
  return $ Block s b

-- Parse a single (possibly nested) symbol, where the nesting can be
-- introduced by one of the given tokens.
tree :: P Tree -> [String] -> P Tree
tree atom intro = atom <|> indent atom intro

-- Parse a continued list of (possibly nested) symbols, where the
-- nesting can be introduced by one of the given tokens.
stride :: P Tree -> [String] -> P Stride
stride atom intro =
  getPos >>= many1 . flip continue (tree atom intro) >>= return . Stride

-- Parse a non-empty sequence of verticaly-aligned strides. Nested
-- blocks can be introduce by one of the given tokens.
block :: P Tree -> [String] -> P [Stride]
block atom intro = aligned (stride atom intro)

-- The top-level parser to parse a non-empty sequence of strides.
-- Nested blocks can be introduce by one of the given tokens.
strides' :: P Tree -> [String] -> String -> Either ParseError [Stride]
strides' atom intro = parse (spaces >> block atom intro) "strides"

-- The top-level parser to parse a non-empty sequence of
-- strides and return them already flattened, using the indent,
-- dedent, and sequence tokens i, d and sq.
-- Nested blocks can be introduce by one of the given tokens.
strides :: P Tree -> [String] -> String -> String -> String ->
  String -> Either ParseError [String]
strides atom intro i d sq = flip parse "strides" $
  spaces >> fmap (flip (flatten i d sq) []) (block atom intro)


