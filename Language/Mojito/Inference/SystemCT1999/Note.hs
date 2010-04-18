module Language.Mojito.Inference.SystemCT1999.Note where

-- Data structure to hold "notes" to "log" the System CT type
-- checking, to be diplayed later.

import Language.Mojito.Syntax.Expr
import Language.Mojito.Inference.SystemCT1999.Context

data Note =
    NString String
  | NId Int String Context (Constrained,Context)
  | NLet Int String (Expr Int) (Expr Int) Context (Maybe (Constrained,Context))
  | NFun Int String (Expr Int) Context (Constrained,Context)
  | NApp Int (Expr Int) (Expr Int) Context (Maybe (Constrained,Context))
  | NHasType Int (Expr Int) Simple Context (Maybe (Constrained,Context))
  | NIf Int (Expr Int) (Expr Int) (Expr Int) Context (Maybe (Constrained,Context))

