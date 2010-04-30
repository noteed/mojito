module Language.Mojito.Inference.SystemCT1999.Narrative where

-- The Narrative module exposes all Mojito's processing,
-- from parsing a string into an s-expression, to the
-- System CT typechecking.
-- This is a bit tedious -- the main data structure here
-- contains variants that grow as the processing is done,
-- but makes things clear.

import Control.Arrow (second)

import Language.Mojito.Syntax.SExpr (SExpr, parseSExprs')
import Language.Mojito.Syntax.Expr (Expr)
import Language.Mojito.Syntax.Types (Simple, showSimple, typed)
import Language.Mojito.Syntax.ExprBuilder
import Language.Mojito.Inference.SystemCT1999.SystemCT1999
import Language.Mojito.Inference.Context
import Language.Mojito.Inference.SystemCT1999.Prelude (someTypes, someContext, someEnvironment)

-- The last string field of each variant is the reason why the
-- processing stoped at that step.
data Narrative =
    -- can't even parse the string
    StepNone String String
    -- parse a string into a list of s-expressions
  | StepSExpr String [SExpr] String
    -- transform to an expression for typechecking
  | StepExpr String [SExpr] (Expr Int) String
    -- typecheck the expression
  | StepTypedExpr String [SExpr] (Expr Int)
    [Simple] Context (Expr Simple)

narrativeOk :: Narrative -> Bool
narrativeOk (StepTypedExpr _ _ _ _ _ _) = True
narrativeOk _ = False

-- Drives the complee processing starting from a
-- string.
buildNarrative :: String -> Narrative
buildNarrative str = case parseSExprs' str of
  Left err -> StepNone str err
  Right sexprs -> case sexprsToExpr sexprs of
    Left err -> StepSExpr str sexprs err
    Right e ->
      let g = someContext
      in case infer' someTypes e g of
        ((Left err, _), _) -> StepExpr str sexprs e err
        ((Right e', _), _) -> StepTypedExpr str sexprs e
          someTypes g e'

showNarrative :: Narrative -> String
showNarrative n = case n of
  StepNone _ err -> "Parse error: " ++ err
  StepSExpr _ _ err -> "Option declaration error: " ++ err
  StepExpr _ _ _ err -> "Type error: " ++ err
  StepTypedExpr _ _ _ _ _ _ -> "Everything's ok."

