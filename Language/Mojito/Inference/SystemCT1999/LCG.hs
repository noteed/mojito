{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Language.Mojito.Inference.SystemCT1999.LCG where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Language.Mojito.Syntax.Types
import Language.Mojito.Prelude.Types

import Language.Mojito.Inference.Substitution
import Language.Mojito.Inference.SystemCT1999.Inferencer

runLcg :: [Type] -> Simple
runLcg ts =
  let Right r = fst $ runIdentity $ evalStateT (runWriterT $ runErrorT $ runInf $ lcg ts) inferencer
  in r

-- Function lcg is implemented along the definition found
-- in ct2.ps.
lcg :: [Type] -> Inf Simple
lcg si = do
  ti <- mapM (liftM smpl . freshQuantified) si
  (t,_) <- lcg' ti []
--  note $ "lcg(" ++ concat (intersperse ", " $ map showType si) ++ ") = " ++ showSimple t
  return t

lcg' :: [Simple] -> [(String,(Simple,Simple))] -> Inf (Simple,[(String,(Simple,Simple))])
lcg' [t] s = return (t,s)

lcg' [t1, t2] s = case reverseLookup (t1,t2) s of
  Just a -> return (TyVar a,s)
  Nothing -> if nargs t1 /= nargs t2
             then do a' <- rename "c"
                     return (TyVar a', s `dag` [(a', (t1, t2))])
             else do let (x ,ts ) = cargs t1 -- FIXME why is there always at least one arg ?
                         (x',ts') = cargs t2
                     (x0,s0) <- if x == x' then return (x,s)
                                           else do a <- rename "d"
                                                   return (TyVar a, s `dag` [(a,(x,x'))])
                     (ti,si) <- lcgAccum ts ts' s0
                     return (foldl TyApp x0 ti, si)

lcg' (t1:t2:ts) s = do
  (t,s0) <- lcg' [t1,t2] s
  (t',s') <- lcg' ts s0
  lcg' [t,t'] s'
lcg' _ _ = error "can't be called"

lcgAccum :: [Simple] -> [Simple] -> [(String, (Simple, Simple))]
  -> Inf ([Simple], [(String, (Simple, Simple))])
lcgAccum [] [] s0 = return ([],s0)
lcgAccum (t1:ts) (t1':ts') s0 = do
  (t1'',s1) <- lcg' [t1,t1'] s0
  (ts2,s) <- lcgAccum ts ts' s1
  return ((t1'' : ts2), s)
lcgAccum _ _ _ = error $ "lcgAccum: not same length; should not happen"

lcgTest1, lcgTest2, lcgTest3, lcgTest4 :: Simple

-- TyApp (TyApp (TyCon "->") (TyCon "int32")) (TyCon "int32")
lcgTest1 = runLcg [Type [] $ Constrained [] $ int32 `fun` int32]

-- TyApp (TyApp (TyCon "->") (TyVar "a0")) (TyVar "a0")
lcgTest2 = runLcg [Type [] $ Constrained [] $ int32 `fun` int32, Type [] $ Constrained [] $ bool `fun` bool]

--TyApp (TyVar "a0") (TyCon "int32")
lcgTest3 =  runLcg [Type [] $ Constrained [] $ tree int32, Type [] $ Constrained [] $ list int32]

-- TyApp (TyVar "a0") (TyVar "a1")
lcgTest4 = runLcg [Type [] $ Constrained [] $ tree (TyVar "a"), Type [] $ Constrained [] $ list (TyVar "b")]

reverseLookup :: Eq a => a -> [(b,a)] -> Maybe b
reverseLookup k = lookup k . map swap
  where
    swap :: (a,b) -> (b,a)
    swap (a,b) = (b,a)

