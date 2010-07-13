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

getPos :: GenParser Char st Pos
getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

-- Continue parsing (using p) after position (l1,c1).
continue :: Pos -> GenParser Char st a -> GenParser Char st a
continue (l1,c1) p = do
  (l2,c2) <- getPos
  unless (c1 < c2 || l1 == l2) pzero
  p

-- aligned p parses many1 p with all p aligned on the same column.
aligned :: GenParser Char st a -> GenParser Char st [a]
aligned p = do
  -- get the defining column
  (_,dc) <- getPos
  -- many1 p but only with p starting exactly at dc
  many1 (getPos >>= \(_,c) -> unless (c == dc) pzero >> p)

keywords :: [String]
keywords = words "let where of"

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for ,()⟨⟩[], which are each a single symbol.
sym :: GenParser Char st Tree
sym = try $ do
  x <- noneOf "\t\n "
  if x `elem` ",()⟨⟩[]"
    then spaces >> return (Sym [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf ",()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords then pzero else spaces >> return (Sym $ x:xs)

-- Parse the empty-list symbol.
empty :: GenParser Char st Tree
empty = try $ do
  _ <- char '['
  spaces
  _ <- char ']'
  spaces
  return $ Sym "[]"

-- Parse a string literal.
str :: GenParser Char st Tree
str = try $ do
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return $ Sym ('"' : x ++ "\"")

-- Parse the string s then a block that starts on the same line
-- or on the same or grater column.
indent :: String -> GenParser Char st Tree
indent s = try $ do
  (l1,c1) <- getPos
  string s >> spaces
  (l2,c2) <- getPos
  unless (c1 <= c2 || l1 == l2) pzero
  b <- block
  return $ Block s b

-- Parse a single (possibly nested) symbol.
tree :: GenParser Char st Tree
tree = str <|> empty <|> sym
  <|> indent "let" <|> indent "where" <|> indent "of"

-- Parse a continued list of (possibly nested) symbols.
stride :: GenParser Char st Stride
stride = getPos >>= many1 . flip continue tree >>= return . Stride

-- Parse a non-empty sequence of verticaly-aligned strides.
block :: GenParser Char st [Stride]
block = aligned stride

-- The top-level parser to parse a non-empty sequence of
-- strides.
strides :: String -> Either ParseError [Stride]
strides = parse (spaces >> block) "strides"

-- The top-level parser to parse a non-empty sequence of
-- strides and return them already flattened, using the indent,
-- dedent, and sequence tokens i, d and sq.
strides' :: String -> String -> String -> String -> Either ParseError [String]
strides' i d sq =
  parse (spaces >> fmap (flip (flatten i d sq) []) block) "strides"

