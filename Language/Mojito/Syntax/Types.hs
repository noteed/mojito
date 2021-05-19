{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
module Language.Mojito.Syntax.Types where

import Prelude hiding ((<>))

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJClass

-- Simple Types
-- t = C t1 .. tn | a t1 .. tn
data Simple =
    TyCon String
  | TyVar String
  | TyApp Simple Simple -- Or Simple [Simple], which would give an arity ?
  deriving (Show, Eq)

instance Pretty Simple where
  pPrint (TyCon c) = text c
  pPrint (TyVar v) = text v
  pPrint (TyApp t1 t2) = text "(" <> pPrint t1 <+> pPrint t2 <> text ")"

showSimple :: Simple -> String
showSimple (TyCon a) = a
showSimple (TyVar a) = a
showSimple (TyApp t1 t2) = "(" ++ showSimple t1 ++ " " ++ showSimple t2 ++ ")"

tc :: Simple -> [String]
-- TODO nub the result, or use union instead of ++
-- tc is also Substitution.vars
tc (TyCon s) = [s]
tc (TyVar _) = []
tc (TyApp t1 t2) = tc t1 ++ tc t2

-- Occur check: tests if a type variable appears in a given
-- type.
occurs :: String -> Simple -> Bool
occurs a (TyVar b) = a == b
occurs _ (TyCon _) = False
occurs a (TyApp t1 t2) = occurs a t1 || occurs a t2

-- Is it faster ?
-- Occur check: tests if a type variable appears in a given
-- type.
-- occurs :: String -> Simple -> Bool
-- occurs a t = a `elem` tv t


tc' :: [Simple] -> [String]
tc' = concatMap tc

args :: Simple -> [Simple]
args (TyCon _) = []
args (TyVar _) = []
args (TyApp t1 t2) = args t1 ++ [t2]

nargs :: Simple -> Int
nargs = length . args

-- t == let (c,as) = cargs t
--      in foldl TyApp c as
cargs :: Simple -> (Simple,[Simple])
cargs t = cargs' t []

cargs' :: Simple -> [Simple] -> (Simple,[Simple])
cargs' t as = case t of
  TyCon _ -> (t,as)
  TyVar _ -> (t,as)
  TyApp t1 t2 -> cargs' t1 (t2 : as)

-- fargs (-> a b c) = [a,b,c]
fargs :: Simple -> [Simple]
fargs (TyCon "->" `TyApp` a `TyApp` b) = a : fargs b
fargs t = [t]

-- Constrained types
-- d = {oi : ti}.t
data Constrained = Constrained [Constraint] Simple
  deriving (Show, Eq)

showConstrained :: Constrained -> String
showConstrained (Constrained [] t) = showSimple t
showConstrained (Constrained cs t) = showConstraints cs ++ "." ++ showSimple t

-- Constraint {o : t} where o is a term variable.
data Constraint = Constraint String Simple
  deriving (Show, Eq)

showConstraint :: Constraint -> String
showConstraint (Constraint o t) = o ++ " : " ++ showSimple t

showConstraints :: [Constraint] -> String
showConstraints cs = "{" ++ concat (intersperse ", " $ map showConstraint cs) ++ "}"

-- Quantified type.
-- s = forall ai . d
data Type = Type [String] Constrained
  deriving (Show, Eq)

showType :: Type -> String
showType (Type [] c) = showConstrained c
showType (Type as c) = "forall " ++ concat (intersperse ", " as) ++ " . " ++ showConstrained c

simple :: Type -> Simple
simple (Type _ (Constrained _ t)) = t

smpl :: Constrained -> Simple
smpl (Constrained _ t) = t

cstr :: Constrained -> [Constraint]
cstr (Constrained c _) = c

typed :: Simple -> Type
typed t = Type [] $ Constrained [] t

isSimple :: Type -> Bool
isSimple (Type [] (Constrained [] _)) = True
isSimple _ = False



