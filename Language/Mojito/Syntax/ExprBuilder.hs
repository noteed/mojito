{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Language.Mojito.Syntax.ExprBuilder where

import Control.Monad.State
import Control.Monad.Error

import Data.List (partition)

import Language.Mojito.Syntax.SExpr
import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types

----------------------------------------------------------------------
-- S-expressions to Expr.
-- TODO The translation between the full language to the core language
-- should be done separately from the buildExpr function.
----------------------------------------------------------------------

data ExprBuilder = ExprBuilder { ebNextId :: Int }

newtype ExprT m a = ExprT { runExprT :: StateT ExprBuilder m a }
  deriving (Monad, MonadIO, MonadTrans, MonadState ExprBuilder, MonadError e)

newId :: Monad m => ExprT m Int
newId = do
  s <- get
  put $ s { ebNextId = ebNextId s + 1 }
  return $ ebNextId s

-- Translates (let ((f x y) a) b) into (let (f (\ x (\y a))) b).
-- Translates only ^^^^^^^^^^^              ^^^^^^^^^^^^^^^^.
-- At least one argument to f should be present.
translateLetWithArgs :: [SExpr] -> [SExpr]
translateLetWithArgs e = case e of
  [List (Sym f:xs@(_:_)), a] ->
    let a' = foldr (\x i -> List [Sym "\\", x, i]) a xs
    in [Sym f, a']
  _ -> error "translateLetWithArgs called on wrong list of s-expressions."

buildExpr :: MonadError String m => SExpr -> ExprT m (Expr Int)
buildExpr e = case e of
  List [Sym "::", a, b] -> do
    i <- newId
    a' <- buildExpr a
    b' <- toSimple b
    return $ HasType i a' b'
  List [Sym "let", List [Sym o, a], b] -> do
    i <- newId
    a' <- buildExpr a
    b' <- buildExpr b
    return $ Let i (Def o a') b'
  List [Sym "let", List a@[List (Sym f:xs@(_:_)), _], b] -> do
    if all isSym xs
      then buildExpr $ List [Sym "let", List $ translateLetWithArgs a, b]
      else throwError $ "Let expression: arguments to "
           ++ f ++ " are not all symbols."
  List [Sym "\\", Sym a, b] -> do
    i <- newId
    b' <- buildExpr b
    return $ Fun i a b'
  List [Sym "if", a, b, c] -> do
    i <- newId
    a' <- buildExpr a
    b' <- buildExpr b
    c' <- buildExpr c
    return $ If i a' b' c'
  List [Sym "case", a, List (b:bs)] -> do
    i <- newId
    a' <- buildExpr a
    b' <- mapM buildAlt (b:bs)
    return $ Case i a' b'
  List [a,b] -> do
    i <- newId
    when (a `elem` [Sym "::", Sym "let", Sym "\\", Sym "if", Sym "case"]) $ 
      throwError $ "unexpected s-expression " ++ show a
    a' <- buildExpr a
    b' <- buildExpr b
    return $ App i a' b'
  List l@(_:_:_) ->
    buildExpr (List [List $ init l,  last l])
  Sym s -> do
    i <- newId
    return $ Id i s
  FltNum s -> do
    i <- newId
    return $ FltLit i s
  IntNum s -> do
    i <- newId
    return $ IntLit i s
  Str s -> do
    i <- newId
    return $ StrLit i s
  _ -> throwError "unexpected s-expression"

toSimple :: MonadError String m => SExpr -> m Simple
toSimple e = case e of
  Sym (h:t) -> do
    if h `elem` ['a'..'z']
      then return $ TyVar (h:t)
      else return $ TyCon (h:t)
  Sym s -> do
    return $ TyCon s
  List [Sym "->", a, b] -> do
    a' <- toSimple a
    b' <- toSimple b
    return $ TyCon "->" `TyApp` a' `TyApp` b'
  List (Sym "->":a:b) -> do
    a' <- toSimple a
    b' <- toSimple (List (Sym "->":b))
    return $ TyCon "->" `TyApp` a' `TyApp` b'
  List [a, b] -> do
    a' <- toSimple a
    b' <- toSimple b
    return $ a' `TyApp` b'
  _ -> throwError "unexpected s-expression for simple type"

buildAlt :: MonadError String m => SExpr -> ExprT m (Alternative Int)
buildAlt e = case e of
  List [pat, expr] -> do
    pat' <- buildPat pat
    expr' <- buildExpr expr
    return $ Alternative pat' expr'
  _ -> throwError $ "unexpected s-expression for alternative " ++ show e

buildPat :: MonadError String m => SExpr -> ExprT m (Pattern Int)
buildPat e = case e of
  List (Sym c:as) -> do
    if not (head c `elem` ['a' .. 'z'])
      then do
        i <- newId
        i' <- newId
        as' <- mapM buildPatOrVar as
        return $ PatCon i i' c as'
      else
        throwError $ "Can't apply a variable in a pattern ("  ++ show e ++ ")."

  Sym c -> do
    if not (head c `elem` ['a' .. 'z'])
      then do
        i <- newId
        i' <- newId
        return $ PatCon i i' c []
      else do
        i <- newId
        return $ PatVar i c

  _ -> throwError $ "unexpected s-expression for pattern "  ++ show e

buildPatOrVar :: MonadError String m => SExpr -> ExprT m (Pattern Int)
buildPatOrVar e = case e of
  List (Sym c:as) -> do
    i <- newId
    i' <- newId
    as' <- mapM buildPatOrVar as
    return $ PatCon i i' c as'
  Sym a -> do
    i <- newId
    return $ PatVar i a
  _ -> throwError $ "unexpected s-expression for pattern "  ++ show e

readExpr :: MonadError String m => String -> m (Expr Int)
readExpr = readDefinitions

readProgram :: MonadError String m => String -> m (Expr Int, [(String, Simple)])
readProgram str = parseSExprs' str >>= sexprToProgram

-- Turns a list of s-expressions into:
-- - an expression (TODO this should be a list of definitions
-- once the typechecking can deal with them, e.g. using Seq/Def);
-- - a list of identifiers for options.
sexprToProgram :: MonadError String m => [SExpr] -> m (Expr Int, [(String, Simple)])
sexprToProgram [] = throwError "At least one s-expression should be give."
-- some special case to handle a single expression
sexprToProgram [List [Sym "=", Sym "main", e]] = do
  e' <- evalStateT (runExprT $ buildExpr e) (ExprBuilder 0)
  return (e', [])
sexprToProgram [List [Sym "=", _, _]] = throwError "Not a definition for 'main'."
sexprToProgram [e] = do
  e' <- evalStateT (runExprT $ buildExpr e) (ExprBuilder 0)
  return (e', [])
-- now the general case (multiple definitions, including one for
-- 'main', option declartions, TODO data type declaration.
sexprToProgram sexprs = do
  (options, sexprs') <- filterOptions sexprs
  sexprs'' <- sexprsToExpr sexprs'
  return (sexprs'', options)

sexprsToExpr :: MonadError String m => [SExpr] -> m (Expr Int)
sexprsToExpr [sexpr] = do
  evalStateT (runExprT $ buildExpr sexpr) (ExprBuilder 0)
sexprsToExpr sexprs = do
  sexprs' <- evalStateT (runExprT $ mapM buildDef sexprs) (ExprBuilder 0)
  return (definitionsToLet sexprs')

-- Each (option type symbol) is removed and the list of
-- (symbol, type) is returned.
filterOptions :: MonadError String m => [SExpr] -> m ([(String, Simple)], [SExpr])
filterOptions sexprs = do
  let (os, sexprs') = partition isOption sexprs
  os' <- mapM toOption os
  return (os', sexprs')
  where isOption (List [Sym "option", _, Sym _]) = True
        isOption _ = False
        toOption (List [Sym "option", t, Sym o]) = do
          t' <- toSimple t
          return (o, t')

-- Read many definitions or just one expression. If only one expression,
-- works just like readExpr. If many definitions, one must be the symbol
-- 'main' and they are translated into a nested Let expression.
readDefinitions :: MonadError String m => String -> m (Expr Int)
readDefinitions str = do
  sexprs <- parseSExprs' str
  case sexprs of
    [] -> throwError "No expression was read."
    [e] -> evalStateT (runExprT $ buildExpr e) (ExprBuilder 0)
    _ -> do
      defs <- evalStateT (runExprT $ mapM buildDef sexprs) (ExprBuilder 0)
      return (definitionsToLet defs)

buildDef :: MonadError String m => SExpr -> ExprT m (String, (Int, Expr Int))
buildDef e = case e of
  List [Sym "=", Sym a, b] -> do
    i <- newId
    b' <- buildExpr b
    return (a, (i, b'))
  List [Sym "=", a@(List (Sym f:xs@(_:_))), b] -> do
    if all isSym xs
      then buildDef $ List $ Sym "=" : translateLetWithArgs [a,b]
      else throwError $ "Let expression: arguments to "
           ++ f ++ " are not all symbols."
  _ -> throwError "The s-expression is not a definition."

definitionsToLet :: [([Char], (a, Expr a))] -> Expr a
definitionsToLet defs = case partition ((== "main") . fst) defs of
  ([],_) -> error "no definition for main."
  (_:_:_,_) -> error "multiple definitions for main."
  ([(_,(_,b))],ds) -> definitionsToLet' b ds

-- Could be a Let/Seq instead of nested Let/Def.
definitionsToLet' :: Expr a -> [(String, (a, Expr a))] -> Expr a
definitionsToLet' m ((a,(i,b)):ds) = Let i (Def a b) $ definitionsToLet' m ds
definitionsToLet' m [] = m

