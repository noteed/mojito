{-# LANGUAGE FlexibleContexts #-}
module Language.Mojito.Inference.Unification where

import Control.Monad.Error

import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Substitution

-- Returns a substitution unifying two types.
unify :: MonadError String m => Simple -> Simple -> m Substitution
unify t1@(TyVar a) t2 | t1 == t2 = return idSubstitution
                      | a `occurs` t2 = throwError $ show t1 ++ " occurs in " ++ show t2
                      | otherwise = return $ replace a t2
unify t1 t2@(TyVar _) = unify t2 t1
unify (TyCon c1) (TyCon c2) | c1 == c2 = return idSubstitution
unify (TyApp a b) (TyApp a' b') = do
  s1 <- unify a a'
  s2 <- unify (subs s1 b) (subs s1 b')
  return (comp "unify" s2 s1)

unify t1 t2 = throwError $ "can't match " ++ show t1 ++ " against " ++ show t2

unify' :: MonadError String m => [(Simple,Simple)] -> m Substitution
unify' [] = return idSubstitution
unify' ((a,b):as) = do
  s1 <- unify a b
  s2 <- unify' (subs s1 as)
  return (comp "unify'"s2 s1)

_unify :: MonadError String m => Simple -> Simple -> [String] -> m Substitution
_unify t1@(TyVar a) t2 vs | a `elem` vs = return idSubstitution -- FIXME: or is it throwError $ a ++ " appears in the free variables" ?
                          | t1 == t2 = return idSubstitution
                          | a `occurs` t2 = throwError $ show t1 ++ " occurs in " ++ show t2
                          | otherwise = return $ replace a t2
_unify t1 t2@(TyVar _) vs = _unify t2 t1 vs
_unify (TyCon c1) (TyCon c2) _ | c1 == c2 = return idSubstitution
_unify (TyApp a b) (TyApp a' b') vs = do
  s1 <- _unify a a' vs
  s2 <- _unify (subs s1 b) (subs s1 b') vs
  return (comp "_unify" s2 s1)

_unify t1 t2 _ = throwError $ "can't match " ++ show t1 ++ " against " ++ show t2

