module Language.Mojito.Syntax.Indent where

import Text.ParserCombinators.Parsec
import Control.Monad (unless)

type Pos = (Int,Int)

getPos :: GenParser Char st Pos
getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

onside :: Pos -> Pos -> Bool
onside (l1,c1) (l2,c2) = c1 < c2 || l1 == l2

offside :: Pos -> GenParser Char st a -> GenParser Char st a
offside pos p = do
  pos' <- getPos
  unless (onside pos pos') pzero
  p

off :: Pos -> GenParser Char st a -> GenParser Char st a
off (_,dc) p = do
  (_,c) <- getPos
  unless (c == dc) pzero
  p

offsideMany1 :: GenParser Char st a -> GenParser Char st [a]
offsideMany1 p = do
  pos <- getPos
  many1 (off pos p)

data Tree =
    Sym String
  | Block String [Stride]
  deriving Show

newtype Stride = Stride [Tree]
  deriving Show

flatten :: [Stride] -> [String] -> [String]
flatten = symStrides
  where
    symStrides [s] = symStride s
    symStrides (s:ss) = symStride s . (";" :) . symStrides ss

    symStride (Stride ts) = symTrees ts

    symTrees [] = id
    symTrees (t:ts) = symTree t . symTrees ts

    symTree (Sym x) = (x :)
    symTree (Block name ss) =
      (\a -> name : "{" : a) .
      symStrides ss .
      ("}" :)

keywords :: [String]
keywords = words "let where of"

sym :: GenParser Char st Tree
sym = try $ do
  x <- noneOf "\t\n "
  if x `elem` "()⟨⟩[]"
    then spaces >> return (Sym [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf ",()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords then pzero else spaces >> return (Sym $ x:xs)

empty :: GenParser Char st Tree
empty = try $ do
  _ <- char '['
  spaces
  _ <- char ']'
  return $ Sym "[]"

str :: GenParser Char st Tree
str = try $ do
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return $ Sym ('"' : x ++ "\"")

block :: String -> GenParser Char st Tree
block s = try $ do
  string s >> spaces
  b <- parseBlock
  return $ Block s b

parseTree :: Pos -> GenParser Char st Tree
parseTree pos = offside pos
  (str <|> empty <|> sym <|> block "let" <|> block "where" <|> block "of")

parseStride :: GenParser Char st Stride
parseStride = getPos >>= many1 . parseTree >>= return . Stride

parseBlock :: GenParser Char st [Stride]
parseBlock = offsideMany1 parseStride

stride :: [Char] -> Either ParseError Stride
stride = parse (spaces >> parseStride) "parseStride"

strides :: [Char] -> Either ParseError [Stride]
strides = parse (spaces >> parseBlock) "parseBlock"

