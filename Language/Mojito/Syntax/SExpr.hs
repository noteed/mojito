{-# LANGUAGE FlexibleContexts #-}
module Language.Mojito.Syntax.SExpr where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad.Error

----------------------------------------------------------------------
-- S-Expressions
----------------------------------------------------------------------

-- An s-expression is either an atom or a list of s-expression.
-- An atom can be a floating or integral number, a string, or
-- a symbol (anything else).
data SExpr = Sym String | FltNum Double | IntNum Integer | Str String
           | List [SExpr]
  deriving (Eq, Show)

isSym :: SExpr -> Bool
isSym (Sym _) = True
isSym _ = False

----------------------------------------------------------------------
-- S-Expressions parsing
----------------------------------------------------------------------

-- Like parseSExpr but turns the Parsec ParseError into a string.
parseSExpr' :: MonadError String m => String -> m SExpr
parseSExpr' s = case parseSExpr s of
  Left err -> throwError $ show err
  Right r -> return r

-- Like parseSExprs but turns the Parsec ParseError into a string.
parseSExprs' :: MonadError String m => String -> m [SExpr]
parseSExprs' s = case parseSExprs s of
  Left err -> throwError $ show err
  Right r -> return r

-- Parse an s-expression and return either a parse error
-- or the parsed s-expression.
parseSExpr :: String -> Either ParseError SExpr
parseSExpr = parse (skipMany blank >> parseExpr) "s-expression"

parseSExprs :: String -> Either ParseError [SExpr]
parseSExprs = parse (many (skipMany blank >> parseExpr)) "s-expressions"

-- Parse a complete symbol.
parseSymbol :: Parser SExpr
parseSymbol = do
  a <- (noneOf " \t\n\"()0123456789")
  b <- many (noneOf " \t\n\"()")
  return $ Sym (a : b)

-- Parse a number, i.e. any symbol beginning with a digit.
parseNumber :: Parser SExpr
parseNumber = (intConstant >>= return . IntNum)
  <|> (floatingConstant >>= return . FltNum)

-- Parse a string in double quotes.
parseString :: Parser SExpr
parseString = do
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  return $ Str x

-- Parse an atom. The () atom is handled by parseList.
parseAtom :: Parser SExpr
parseAtom = parseSymbol <|> parseNumber <|> parseString

-- Parse a list, i.e. many expressions bracketed by parens
-- or the () atom.
parseList :: Parser SExpr
parseList = do
  _ <- char '('
  skipMany blank
  x <- parseExprs
  _ <- char ')'
  return $ if null x then Sym "()" else List x

-- Parse an expression (an atom or a list).
parseExpr :: Parser SExpr
parseExpr = do
  s <- (parseAtom <|> parseList)
  skipMany blank
  return s

-- Parse many expressions (parens not included).
parseExprs :: Parser [SExpr]
parseExprs = many parseExpr

----------------------------------------------------------------------
-- Number parsing (taken from language-glsl).
----------------------------------------------------------------------

-- TODO the size of the int should fit its type.
intConstant :: Parser Integer
intConstant = choice
  [ hexadecimal
  , octal
  , badOctal >> fail "Invalid octal number"
  , decimal
  ]

floatingConstant :: Parser Double
floatingConstant = choice
  [ floatExponent
  , floatPoint
  , pointFloat
  ]

----------------------------------------------------------------------
-- Lexical elements helpers
----------------------------------------------------------------------

comment :: Parser ()
comment = do
  _ <- string "--- "
  _ <- manyTill anyChar ((newline >> return ()) <|> eof)
  return ()

blank :: Parser ()
blank = try comment <|> (space >> return ())

hexadecimal :: Parser Integer
hexadecimal = try $ do
  _ <- char '0'
  _ <- oneOf "Xx"
  d <- many1 hexDigit
  _ <- optionMaybe $ oneOf "Uu" -- TODO
  return $ read ("0x" ++ d)

octal :: Parser Integer
octal = try $ do
  _ <- char '0'
  d <- many1 octDigit
  _ <- optionMaybe $ oneOf "Uu" -- TODO
  return $ read  ("0o" ++ d)

badOctal :: Parser ()
badOctal = try  $ char '0' >> many1 hexDigit >> return ()

decimal :: Parser Integer
decimal = try $ do
  d <- many1 digit
  notFollowedBy (char '.' <|> (expo >> return ' '))
  _ <- optionMaybe $ oneOf "Uu" -- TODO
  return $ read d

floatExponent :: Parser Double
floatExponent = try $ do
  d <- many1 digit
  e <- expo
  _ <- optionMaybe $ oneOf "Ff" -- TODO
  return $ read $ d ++ e

floatPoint :: Parser Double
floatPoint = try $ do
  d <- many1 digit
  _ <- char '.'
  d' <- many digit
  let d'' = if null d' then "0" else d'
  e <- optionMaybe expo
  _ <- optionMaybe $ oneOf "Ff" -- TODO
  return $ read $ d ++ "." ++ d'' ++ maybe "" id e

pointFloat :: Parser Double
pointFloat = try $ do
  _ <- char '.'
  d <- many1 digit
  e <- optionMaybe expo
  _ <- optionMaybe $ oneOf "Ff" -- TODO
  return $ read $ "0." ++ d ++ maybe "" id e

expo :: Parser String
expo = try $ do
  _ <- oneOf "Ee"
  s <- optionMaybe (oneOf "+-")
  d <- many1 digit
  return $ "e" ++ maybe "" (:[]) s ++ d

