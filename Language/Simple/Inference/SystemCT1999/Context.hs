module Language.Simple.Inference.SystemCT1999.Context where

import Data.List (union)

import Language.Simple.Syntax.Expr (Type, showType)

-- Typing context Gamma.
-- Each element is a pair (term variable,s) called 'typing'.
-- Multiple typings can be given to let-bound variable.
data Context = Context {
    ctxAssoc :: [(String,Type)]
  }

showContext :: Context -> String
showContext g = unlines $ map (\(a,b) -> a ++ " : " ++ showType b) (ctxAssoc g)

types :: Context -> String -> [Type]
types g x = map snd $ filter ((== x) . fst) (ctxAssoc g)

unionContexts :: [Context] -> Context
unionContexts gs = Context (foldl union [] $ map ctxAssoc gs)


