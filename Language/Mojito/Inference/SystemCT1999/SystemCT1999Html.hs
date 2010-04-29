module Language.Mojito.Inference.SystemCT1999.SystemCT1999Html where

import Data.Maybe (fromJust)
import Data.List (intersperse)
import Text.XHtml.Strict

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Context
import Language.Mojito.Inference.Substitution (subs, showSubstitution)
import Language.Mojito.Inference.SystemCT1999.Note
import Language.Mojito.Inference.SystemCT1999.SystemCT1999

htmlNote :: Note -> Html
htmlNote n = case n of
  NString m ->
    thediv (stringToHtml m) ! [theclass "note"]
  NId key x g (c,g') ->
    thediv (stringToHtml $ "Id " ++ show key ++ " " ++ x) ! [theclass "note"] +++
    thediv (htmlConstrained c) +++
    thediv (htmlContextHighlight x g) ! [theclass "left"] +++
    thediv (htmlContext g') ! [theclass "right"]
  NLet key o _ _ g mcg' ->
    thediv (stringToHtml $ "Let " ++ show key ++ " " ++ o) ! [theclass "note"] +++
    thespan (htmlContext g) ! [theclass "left" ] +++
    (maybe noHtml ((! [theclass "right"]) . thespan . htmlContext . snd) mcg')
  NFun key u _ g (c,g') ->
    thediv (stringToHtml $ "Fun " ++ show key ++ " " ++ u) ! [theclass "note"] +++
    thediv (htmlConstrained c) +++
    thediv (htmlContext g) ! [theclass "left"] +++
    thediv (htmlContext g') ! [theclass "right"]
  NApp key _ _ g mcg' ->
    thediv (stringToHtml $ "App " ++ show key) ! [theclass "note"] +++
    (maybe noHtml (htmlConstrained . fst) mcg') +++
    thespan (htmlContext g) ! [theclass "left" ] +++
    (maybe noHtml ((! [theclass "right"]) . thespan . htmlContext . snd) mcg')
  NHasType key _ _ _ _ ->
    thediv (stringToHtml $ "HasType " ++ show key) ! [theclass "note"]
  NIf key _ _ _ g mcg' ->
    thediv (stringToHtml $ "If " ++ show key) ! [theclass "note"] +++
    (maybe noHtml (htmlConstrained . fst) mcg') +++
    thespan (htmlContext g) ! [theclass "left" ] +++
    (maybe noHtml ((! [theclass "right"]) . thespan . htmlContext . snd) mcg')

htmlContext :: Context -> Html
htmlContext g = (defList $ map f $ ctxAssoc g) ! [theclass "context"]
  where f (a,b) = (stringToHtml a, htmlType b)

-- It would be nice to also highlight the entry for x'constraints (if any)
htmlContextHighlight :: String -> Context -> Html
htmlContextHighlight x g = (dlist $ toHtmlFromList $ map f $ ctxAssoc g) ! [theclass "context"]
  where f (a,b) | a == x = dterm (stringToHtml a) ! [theclass "highlight"] +++ ddef (htmlType b)
                | otherwise = dterm (stringToHtml a) +++ ddef (htmlType b)

htmlType :: Type -> Html
htmlType = stringToHtml . showType

htmlConstrained :: Constrained -> Html
htmlConstrained = stringToHtml . showConstrained

writeHtmlReport :: [Simple] -> Context -> String -> IO ()
writeHtmlReport ts g s =
  writeFile "report.html" (htmlReport "report" $ report ts g s)

htmlReport :: String -> Report -> String
htmlReport t r =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" ++
  "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n" ++
  "<head>\n" ++
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"my.css\"/>\n" ++
  "<title>" ++ t ++ "</title>\n</head><body>\n" ++

  (showHtml $ htmlReportBody r) ++

  "\n</body></html>\n"

htmlReportBody  :: Report -> Html
htmlReportBody (NoReport code err n) = thediv $
  sectionCode "Code" code +++
  sectionCode "Error" err +++
  sectionHtml "Notes" (map htmlNote n)

htmlReportBody r = thediv $
  sectionCode "Maximal type" (showConstrained $ rType r) +++
--  sectionCode "Minimal context" (showContext $ rContext r) +++
  sectionCode "sat_c (constraints maximal type, minimal context)" (showSubstitutions $ satc (cstr $ rType r) (rContext r)) +++
--  sectionCode "Initial context" (showContext $ rInitialContext r) +++
--  sectionCode "Code" (rCode r) +++
  sectionCode "Expression" (showExpr 0 showExprType $ rExpr r) +++
  sectionCode "Expression" (showExpr 0 showExprType' $ e') +++
  sectionCode "Expression'" (showExpr 0 showExprType' $ e'') +++
  sectionCode "Expression" (show $ rExpr r) +++
--  sectionCode "Typings" (showTypings $ rTypings r) +++
  sectionCode "Substitution" (showSubstitution $ rSubstitution r) +++
  sectionHtml "Notes" (map (thediv . htmlNote) $ rNotes r)

  where
--        showTypings = unlines . map (\(a,(b,c)) -> show a ++ " : " ++ showConstrained (rSubstitution r `subs` b) ++ "\n" ++ showContext c)
        showSubstitutions = unlines . map showSubstitution
        showExprType k = " : " ++ showConstrained (rSubstitution r `subs` (fst . fromJust $ lookup k (rTypings r)))
        e' = giveTypes (rExpr r) (rSubstitution r) (rTypings r)
        e'' = giveTypes' (rExpr r) (rSubstitution r) (rTypings r)
        showExprType' k = " : " ++ concat (intersperse ", " $ map showSimple k)

sectionCode :: String -> String -> Html
sectionCode t c = h2 (stringToHtml t) +++ pre (thecode $ stringToHtml c)

sectionHtml :: HTML a => String -> a -> Html
sectionHtml t h = h2 (stringToHtml t) +++ h


