{-# LANGUAGE FlexibleContexts #-}
-- Milner's type checking in Haskell, from
-- Cardelli's paper Basic Polymorphic Typechecking, 1987

module Language.Mojito.Inference.Cardelli.Cardelli where

import Control.Monad.State

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Substitution
import qualified Language.Mojito.Inference.Unification as U
import Language.Mojito.Inference.Cardelli.Environment
import Language.Mojito.Inference.Cardelli.Inferencer

unify :: Simple -> Simple -> Substitution
unify a b = case U.unify a b of
  Left err -> error err
  Right s -> s

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

-- Given a type, returns the same type with all the
-- generic variables renamed with fresh names.
fresh :: Simple -> [String] -> State Inferencer Simple
fresh t ng = do
  let gs = gvars t ng
  gs' <- mapM rename gs
  return $ subs (zip gs gs') t

fun :: Simple -> Simple -> Simple
fun a b = TyCon "->" `TyApp` a `TyApp` b

bool :: Simple
bool = TyCon "bool"

int :: Simple
int = TyCon "int"

pair :: Simple -> Simple -> Simple
pair a b = TyCon "," `TyApp` a `TyApp` b

u :: a
u = undefined

true, false, one, two :: Expr a
true = Id u "true"
false = Id u "false"
one = Id u "1"
two = Id u "2"

initialEnv :: Env
initialEnv =
  [ ("true", bool)
  , ("false", bool)
  , ("1", int)
  , ("2", int)
  , ("mkPair", fun (TyVar "a") (fun (TyVar "b") (pair (TyVar "a") (TyVar "b"))))
  , ("fst", fun (pair (TyVar "a") (TyVar "b")) (TyVar "a"))
  , ("snd", fun (pair (TyVar "a") (TyVar "b")) (TyVar "b"))
  , ("iszero", fun int bool)
  , ("+", fun int (fun int int))
  ]

-- analyzeExpr e tenv ng typingState
-- e: the expression to type
-- tenv: the type environment
-- ng: the non-generic variables
-- typingState: state monad used to perform the typing
analyzeExpr :: Expr a -> Env -> [String] -> State Inferencer Simple
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
    compose $ unify t1 bool
    t2 <- analyzeExpr e2 env ng
    t3 <- analyzeExpr e3 env ng
    compose $ unify t2 t3
    substitute t2
  Fun _ x e2 -> do
    t1@(TyVar v) <- rename x
    let env' = extend x t1 env
        ng' = v : ng
    t2 <- analyzeExpr e2 env' ng'
    substitute (fun t1 t2)
  App _ e1 e2 -> do
    t1 <- analyzeExpr e1 env ng
    t2 <- analyzeExpr e2 env ng
    t3 <- rename "return"
    compose $ unify t1 (fun t2 t3)
    substitute t3
  Let _ decl e1 -> do
    note "Let"
    env' <- analyzeDecl decl env ng
    analyzeExpr e1 env' ng
  Case _ _ _ -> error "TODO Case"
  HasType _ _ _ -> error "TODO HasType"

analyzeDecl :: Decl a -> Env -> [String] -> State Inferencer Env
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

analyzeRecBind :: Decl a -> Env -> [String] -> State Inferencer (Env, [String])
analyzeRecBind decl env ng = case decl of
  Def x _ -> do
    note "Def (bind)"
    t1@(TyVar v) <- rename x
    note $ x ++ " is renamed " ++ v
    return (extend x t1 env, v : ng)
  Seq d1 d2 -> do
    (env', ng') <- analyzeRecBind d1 env ng
    analyzeRecBind d2 env' ng'
  Rec d -> analyzeRecBind d env ng

analyzeRec :: Decl a -> Env -> [String] -> State Inferencer ()
analyzeRec decl env ng = case decl of
  Def x e1 -> do
    note "Def"
    t <- retrieve x env ng
    note $ x ++ " has type " ++ show t
    t1 <- analyzeExpr e1 env ng
    note $ "and will be unified with " ++ show t1
    compose $ unify t t1
    s <- gets tiSubstitution
    note $ "the substitution is now " ++ show s
  Seq d1 d2 -> do
    analyzeRec d1 env ng
    analyzeRec d2 env ng
  Rec d -> analyzeRec d env ng

typeExpr :: Expr a -> (Simple, Inferencer)
typeExpr e = (t,s)
  where (t,s) = runState (analyzeExpr e initialEnv []) inferencer

infer :: Env -> Expr a -> Simple
infer env e = evalState (analyzeExpr e env []) inferencer

