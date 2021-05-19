{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
module Language.Mojito.Syntax.Expr where

import Prelude hiding ((<>))

import Data.List (intersperse, nub)
import Text.PrettyPrint.HughesPJClass

import Language.Mojito.Syntax.Types

-- The expressions from Cardelli's paper are
-- e ::= x | if e e e | fun x e | e e | let decl e
-- The Camarao paper has only
-- e ::= x | fun x e | e e | let x = e in e
-- There is (in both) a let construct for polymorphic bindings
-- (lambda-bound identifiers are not polymorphic).
data Expr k =
    Id k String
  | FltLit k Double
  | IntLit k Integer
  | StrLit k String
  | If k (Expr k) (Expr k) (Expr k)
  | Fun k String (Expr k)
  | App k (Expr k) (Expr k)
  | Let k (Decl k) (Expr k)
  | Case k (Expr k) [Alternative k]
  | HasType k (Expr k) Simple
  deriving Show

-- Returns the free variables of an expression.
class FV a where
  fv :: [String] -> a -> [String]

-- Returns the free variables of an expression.
class FV' e a where
  fv' :: [(String,a)] -> e a -> [(String,a)]

instance FV (Expr k) where
  fv env e = nub $ case e of
    Id _ s -> if s `elem` env then [] else [s]
    FltLit _ _ -> []
    IntLit _ _ -> []
    StrLit _ _ -> []
    If _ e1 e2 e3 -> concatMap (fv env) [e1,e2,e3]
    Fun _ s e1 -> fv (s:env) e1
    App _ e1 e2 -> concatMap (fv env) [e1,e2]
    Let _ (Def x e1) e2 -> fv env e1 ++ fv (x:env) e2 -- not a recursive let
    Case _ e1 as -> fv env e1 ++ concatMap (fv env) as
    HasType _ e1 _ -> fv env e1

instance FV' Expr Simple where
  fv' env e = nub $ case e of
    Id k s -> if (s,k) `elem` env then [] else [(s,k)]
    FltLit _ _ -> []
    IntLit _ _ -> []
    StrLit _ _ -> []
    If _ e1 e2 e3 -> concatMap (fv' env) [e1,e2,e3]
    Fun (TyApp (TyCon "->" `TyApp` t) _) s e1 -> fv' ((s,t):env) e1
    Fun _ _ _ -> error "not a fun type"
    App _ e1 e2 -> concatMap (fv' env) [e1,e2]
    Let _ (Def x e1) e2 -> fv' env e1 ++ fv' ((x,unExpr e1):env) e2 -- not a recursive let
    Case _ e1 as -> fv' env e1 ++ concatMap (fv' env) as
    HasType _ e1 _ -> fv' env e1

data Alternative k = Alternative (Pattern k) (Expr k)
  deriving Show

instance Pretty k => Pretty (Alternative k) where
  pPrint (Alternative p e) = text "(" <> nest 2 (vcat [pPrint p, pPrint e]) <> text ")"

instance FV (Alternative k) where
  fv env (Alternative p e) = fv (map fst (patVars p) ++ env) e

instance FV' Alternative Simple where
  fv' env (Alternative p e) = fv' (patVars p ++ env) e

-- Right is for another pattern (beginning by a constructor) and
-- Left is for a variable.
-- e.g. Just [a] is Pattern "Just" [Right $ Pattern "[]" [Left "a"]]
data Pattern k = PatVar k String | PatCon k k String [Pattern k]
  deriving Show

instance Pretty k => Pretty (Pattern k) where
  pPrint (PatVar k v) = text v <+> text ":" <+> pPrint k
  pPrint (PatCon _ k' c ps) = text c <+> text ":" <+> vcat (pPrint k' : map pPrint ps)

-- Given a pattern, returns the list of variables appearing in it.
patVars :: Pattern t -> [(String,t)]
patVars (PatVar s v) = [(v,s)]
patVars (PatCon _ _ _ as) = concatMap patVars as

-- Given a pattern, returns an expression corresponding to it.
patExpr :: Pattern Int -> Expr Int
patExpr (PatVar k v) = Id k v
patExpr (PatCon _ k c []) = Id k c
patExpr (PatCon k k' c (a:as)) = foldl g (App k (Id k' c) (patExpr a)) as
  where g l r = App (-1) l (patExpr r)

instance Functor Expr where
  fmap f e = case e of
    Id k s -> Id (f k) s
    FltLit k l -> FltLit (f k) l
    IntLit k l -> IntLit (f k) l
    StrLit k l -> StrLit (f k) l
    If k e1 e2 e3 -> If (f k) (fmap f e1) (fmap f e2) (fmap f e3)
    Fun k s e1 -> Fun (f k) s (fmap f e1)
    App k e1 e2 -> App (f k) (fmap f e1) (fmap f e2)
    Let k d e1 -> Let (f k) (fmap f d) (fmap f e1)
    Case k e1 as -> Case (f k) (fmap f e1) (map (fmap f) as)
    HasType k e1 s -> HasType (f k) (fmap f e1) s

instance Functor Alternative where
  fmap f (Alternative p e) = Alternative (fmap f p) (fmap f e)

instance Functor Pattern where
  fmap f (PatVar k v) = PatVar (f k) v
  fmap f (PatCon k k' c as) = PatCon (f k) (f k') c (map (fmap f) as)

unExpr :: Expr k -> k
unExpr e = case e of
  Id k _ -> k
  FltLit k _ -> k
  IntLit k _ -> k
  StrLit k _ -> k
  If k _ _ _ -> k
  Fun k _ _ -> k
  App k _ _ -> k
  Let k _ _ -> k
  Case k _ _ -> k
  HasType k _ _ -> k

-- The declarations from Cardelli are
-- d ::= x = e | d ; d | rec d
-- Multiple defintions can be given in a single let construct.
-- The definitions can be (mutually) recursive.
-- In Camaro, there is only
-- d ::= x = e
-- Moreover, they treat multiple (nested) let-in as being
-- non-nested. So to reuse the data type from Cardelli.hs,
-- the declarations will be like Let (Seq (Def ..) (Def ..)) ..
data Decl k =
    Def String (Expr k)
  | Seq (Decl k) (Decl k)
  | Rec (Decl k)
  deriving Show

instance Functor Decl where
  fmap f d = case d of
    Def s e -> Def s (fmap f e)
    Seq d1 d2 -> Seq (fmap f d1) (fmap f d2)
    Rec d1 -> Rec (fmap f d1)

instance Pretty k => Pretty (Expr k) where
  pPrint e = case e of
    Id k s -> text s <+> text ":" <+> pPrint k
    FltLit k l -> pPrint l <+> text ":" <+> pPrint k
    IntLit k l -> pPrint l <+> text ":" <+> pPrint k
    StrLit k l -> pPrint l <+> text ":" <+> pPrint k
    If k e1 e2 e3 -> text "(if" <+> text ":" <+> pPrint k $$ nest 2 (vcat [pPrint e1, pPrint e2, pPrint e3]) <> text ")"
    Fun k s e1 -> text "(\\" <+> text ":" <+> pPrint k $$ nest 2 (vcat [text s, pPrint e1]) <> text ")"
    App k e1 e2 -> text "(" <+> text ":" <+> pPrint k $$ nest 2 (vcat [pPrint e1, pPrint e2]) <> text ")"
    Let k (Def o e1) e2 -> text "(let" <+> text ":" <+> pPrint k $$ nest 2 (vcat [text "(" <> vcat [text o, pPrint e1] <> text ")", pPrint e2]) <> text  ")"
    Let _ _ _ -> error "unexpected Let expression"
    Case k e1 as -> text "(case" <+> text ":" <+> pPrint k $$ nest 2 (vcat $ pPrint e1 : map pPrint as) <> text ")"
    HasType k e1 t -> text "(::" <+> text ":" <+> pPrint k $$ nest 2 (vcat [pPrint e1, pPrint t]) <> text ")"
 

showExpr :: Int -> (k -> String) -> Expr k -> String
showExpr i f e = case e of
  Id k s -> sp s ++ f k
  FltLit k l -> sp (show l) ++ f k
  IntLit k l -> sp (show l) ++ f k
  StrLit k l -> sp (show l) ++ f k
  If k e1 e2 e3 -> sp "(if" ++ f k ++ "\n" ++ concat (intersperse "\n" $ map (showExpr (i + 1) f) [e1,e2,e3]) ++ ")"
  Fun k s e1 -> sp "(\\" ++ f k ++ "\n" ++ sp "  " ++ s ++ "\n" ++ showExpr (i + 1) f e1 ++ ")"
  App k e1 e2 -> sp "(" ++ f k ++ "\n" ++ showExpr (i + 1) f e1 ++ "\n" ++ showExpr (i + 1) f e2 ++ ")"
  Let k (Def o e1) e2 -> sp "(let" ++ f k ++ "\n" ++ sp "  (" ++ o ++ "\n" ++ showExpr (i + 1) f e1 ++ ")\n" ++ showExpr (i + 1) f e2 ++ ")"
  Let _ _ _ -> error "unexpected Let expression"
  Case k e1 as -> sp "(case" ++ f k ++ "\n" ++ showExpr (i + 1) f e1 ++ "\n" ++ concat (intersperse "\n" $ map (showAlt f) as) ++ ")"
  HasType k e1 t -> sp "(::" ++ f k ++ "\n" ++ showExpr (i + 1) f e1 ++ "\n" ++ sp "  " ++ showSimple t ++ ")"
  where sp str = replicate (i * 2) ' ' ++ str

showExpr' :: Int -> (k -> String) -> Expr k -> String
showExpr' i f e = sp ++ case e of
  Id _ s -> s
  FltLit _ l -> show l
  IntLit _ l -> show l
  StrLit _ l -> show l
  If _ e1 e2 e3 -> "(if " ++ concat (intersperse " " $ map (showExpr' i f) [e1,e2,e3]) ++ ")"
  Fun _ s e1 -> "(\\ " ++ s ++ " " ++ showExpr' i f e1 ++ ")"
  App _ e1 e2 -> "(" ++ showExpr' i f e1 ++ " " ++ showExpr' i f e2 ++ ")"
  Let _ (Def o e1) e2 -> "(let (" ++ o ++ " " ++ showExpr' i f e1 ++ ")\n" ++ showExpr' (i + 1) f e2 ++ ")"
  Let _ _ _ -> error "unexpected Let expression"
  Case _ e1 as -> "(case " ++ showExpr' i f e1 ++ "\n" ++ "TODO:show alternatives)"
  HasType _ e1 t -> "(:: " ++ showExpr' i f e1 ++ " " ++ showSimple t ++ ")"
  where sp = replicate (i * 2) ' '

showAlt :: (t -> String) -> Alternative t -> String
showAlt f (Alternative p e) = showPat f p ++ " -> " ++ showExpr 0 f e

showPat :: (t -> String) -> Pattern t -> String
showPat f (PatVar k v) = v ++ " : " ++ f k
showPat f (PatCon _ k2 c ps) = c ++ " : " ++ f k2 ++ concatMap (showPat f) ps

