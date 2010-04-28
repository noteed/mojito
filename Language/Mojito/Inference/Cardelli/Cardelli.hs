{-# LANGUAGE FlexibleContexts #-}
-- Milner's type checking in Haskell, from
-- Cardelli's paper Basic Polymorphic Typechecking, 1987

module Language.Mojito.Inference.Cardelli.Cardelli where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Prelude.Types
import Language.Mojito.Inference.Unification
import Language.Mojito.Inference.Cardelli.Environment
import Language.Mojito.Inference.Cardelli.Inferencer

----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------

infer :: Expr a -> Env -> ((Either String Simple, [Note]), Inferencer)
infer e env = runIdentity $
  runStateT
    (runWriterT . runErrorT . runInf $ analyzeExpr e env [])
    inferencer

-- e ::= x | if e e e | fun x e | e e | let decl e
-- There is a let construct for polymorphic bindings
-- (lambda-bound identifiers are not polymorphic).

-- d ::= x = e | d ; d | rec d
-- Multiple defintions can be given in a single let construct.
-- The definitions can be (mutually) recursive.

-- A type is either a type variable or a type operator with
-- some arguments. Functions, pairs, ... are represented with
-- such operators. There is no type environment closure. The
-- distinction between generic variables (which should be
-- freshly renamed to enable polymorphism) is done by the use
-- of list of non-generic variables (expanded in a similar way
-- to the environment).

-- A (type) substitution is a function from type variables to
-- types that differs from the identity function only on finitely
-- many variable (definition from Camarao 1999, Type Inference for
-- Overloading without Restrictions, Declarations or Annotations).
-- It is represented by an association list. Applying the
-- substitution to a type is done by looking up the type from the
-- association-list. If it is not found, it is returned as-is.
-- A substitution can also be applied to a type environment.
-- Instead of modifying the environment each time a new substitution
-- is computed, the substitution is applied when an identifer is
-- looked up (see the function 'retrieve').

-- Retrieves the type corresponding to an identifier,
-- giving fresh names to its generic variables, and
-- applying the current substitution.
-- This function is System CT's pt couterpart.
retrieve :: String -> Env -> [String] -> Inf Simple
retrieve a tenv ng = case lookup a tenv of
  Nothing -> error $ "unbound type variable " ++ a
  Just t -> do
    -- the substitution should be applied before renaming
    -- (a variable v can be renamed into v0 while the
    -- substitution turns it into bool (which won't be
    -- renamed).
    t' <- substitute t
    refresh t' ng

-- analyzeExpr e tenv ng typingState
-- e: the expression to type
-- tenv: the type environment
-- ng: the non-generic variables
-- typingState: state monad used to perform the typing
analyzeExpr :: Expr a -> Env -> [String] -> Inf Simple
analyzeExpr e env ng = case e of
  Id _ a -> do
    note "Id"
    t <- retrieve a env ng
    note $ a ++ " has type " ++ show t
    return t
  FltLit _ a -> do
    note "FltLit"
    t <- retrieve (show a) env ng
    note $ show a ++ " has type " ++ show t
    return t
  IntLit _ a -> do
    note "IntLit"
    t <- retrieve (show a) env ng
    note $ show a ++ " has type " ++ show t
    return t
  StrLit _ a -> do
    note "StrLit"
    t <- retrieve (show a) env ng
    note $ show a ++ " has type " ++ show t
    return t
  If _ e1 e2 e3 -> do
    t1 <- analyzeExpr e1 env ng
    s1 <- unify t1 bool
    compose s1
    t2 <- analyzeExpr e2 env ng
    t3 <- analyzeExpr e3 env ng
    s2 <- unify t2 t3
    compose s2
    substitute t2
  Fun _ x e2 -> do
    t1@(TyVar v) <- fresh x
    let env' = extend x t1 env
        ng' = v : ng
    t2 <- analyzeExpr e2 env' ng'
    substitute (fun t1 t2)
  App _ e1 e2 -> do
    t1 <- analyzeExpr e1 env ng
    t2 <- analyzeExpr e2 env ng
    t3 <- fresh "return"
    s <- unify t1 (fun t2 t3)
    compose s
    substitute t3
  Let _ decl e1 -> do
    note "Let"
    env' <- analyzeDecl decl env ng
    analyzeExpr e1 env' ng
  Case _ _ _ -> error "TODO Case"
  HasType _ _ _ -> error "TODO HasType"

analyzeDecl :: Decl a -> Env -> [String] -> Inf Env
analyzeDecl decl env ng = case decl of
  Def x e1 -> do
    t1 <- analyzeExpr e1 env ng
    return $ extend x t1 env
  Seq d1 d2 -> do
    env' <- analyzeDecl d1 env ng
    analyzeDecl d2 env' ng
  Rec d -> do
    note "Rec"
    (env', ng') <- analyzeRecBind d env ng
    analyzeRec d env' ng'
    return env'

analyzeRecBind :: Decl a -> Env -> [String] -> Inf (Env, [String])
analyzeRecBind decl env ng = case decl of
  Def x _ -> do
    note "Def (bind)"
    t1@(TyVar v) <- fresh x
    note $ x ++ " is renamed " ++ v
    return (extend x t1 env, v : ng)
  Seq d1 d2 -> do
    (env', ng') <- analyzeRecBind d1 env ng
    analyzeRecBind d2 env' ng'
  Rec d -> analyzeRecBind d env ng

analyzeRec :: Decl a -> Env -> [String] -> Inf ()
analyzeRec decl env ng = case decl of
  Def x e1 -> do
    note "Def"
    t <- retrieve x env ng
    note $ x ++ " has type " ++ show t
    t1 <- analyzeExpr e1 env ng
    note $ "and will be unified with " ++ show t1
    s1 <- unify t t1
    compose s1
    s2 <- gets tiSubstitution
    note $ "the substitution is now " ++ show s2
  Seq d1 d2 -> do
    analyzeRec d1 env ng
    analyzeRec d2 env ng
  Rec d -> analyzeRec d env ng

