module Language.Mojito.Inference.SystemCT1999.NarrativeHtml where

import Text.XHtml.Strict hiding (input, label, name, p)

import Language.Mojito.Inference.SystemCT1999.Prelude (someEnvironment)
import Language.Mojito.Inference.SystemCT1999.Narrative

htmlNarrative :: Narrative -> Html
htmlNarrative n = case n of
  StepNone _ err -> htmlError "Parse error" err
  StepSExpr _ _ err -> htmlError "S-expression error" err
  StepExpr _ _ _ err -> htmlError "Type error" err
  StepTypedExpr str sexprs exp ts g exp' -> thediv $
    thediv (stringToHtml "Everything's ok.") ! [theclass "success-message"]
    +++ sourceCode str
    +++ originalSExpr (show sexprs)
    +++ untypedExpr (show exp)
    +++ typedExpr (show exp')

htmlError err1 err2 = thediv
  (thespan (stringToHtml err1) +++ thespan (stringToHtml err2)) !
  [theclass "error-message"]

sourceCode str = thediv $
  thespan (stringToHtml "source code")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-source-code", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "source-code", theclass "box"]

originalSExpr str = thediv $
  thespan (stringToHtml "original s-expression")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-original-sexpr", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "original-sexpr", theclass "box"]

programOptions str = thediv $
  thespan (stringToHtml "program options")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-program-options", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "program-options", theclass "box"]

processedSExpr str = thediv $
  thespan (stringToHtml "processed s-expression")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-processed-sexpr", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "processed-sexpr", theclass "box"]

untypedExpr str = thediv $
  thespan (stringToHtml "untyped expression")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-untyped-expr", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "untyped-expr", theclass "box"]

typedExpr str = thediv $
  thespan (stringToHtml "typed expression")
  +++ anchor (stringToHtml "show/hide") ! [identifier "toggle-typed-expr", href "#"]
  +++ thediv (pre (thecode $ stringToHtml str)) ! [identifier "typed-expr", theclass "box"]

