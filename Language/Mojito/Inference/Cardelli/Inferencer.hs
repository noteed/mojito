{-# LANGUAGE FlexibleContexts #-}
module Language.Mojito.Inference.Cardelli.Inferencer where

import Data.List ((\\))
import Control.Monad.State

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Substitution
import qualified Language.Mojito.Inference.Unification as U
import Language.Mojito.Inference.Cardelli.Environment

data Inferencer = Inferencer
  { tiNextId :: Int
  , tiSubstitution :: Substitution
--  , notes :: [String]
  }
  deriving Show

inferencer :: Inferencer
inferencer = Inferencer
  { tiNextId = 0
  , tiSubstitution = idSubstitution
--  , notes = []
  }

--note :: MonadState Inferencer m => String -> m ()
--note m = modify (\s -> s { notes = m : notes s })
note :: Monad m => String -> m ()
note _ = return ()

-- Creates a unique type variable from a string.
rename :: String -> State Inferencer Simple
rename a = do
  n <- gets tiNextId
  modify (\s -> s { tiNextId = n + 1 })
  return (TyVar $ a ++ show n)

-- Compose the given substitution with the current substitution.
compose :: Substitution -> State Inferencer ()
compose ts = do
--  note ts
  n <- gets tiSubstitution
  modify (\s -> s { tiSubstitution = comp "compose" ts n })

-- Returns a type using the current substitution.
substitute :: MonadState Inferencer m => Simple -> m Simple
substitute t = do
  n <- gets tiSubstitution
  return (subs n t)

-- Given a type, returns the same type with all the
-- generic variables renamed with fresh names.
fresh :: Simple -> [String] -> State Inferencer Simple
fresh t ng = do
  let gs = gvars t ng
  gs' <- mapM rename gs
  return $ subs (zip gs gs') t

-- Retrieves the type corresponding to an identifier,
-- giving fresh names to its generic variables, and
-- applying the current substitution.
retrieve :: String -> Env -> [String] -> State Inferencer Simple
retrieve a tenv ng = case lookup a tenv of
  Nothing -> error $ "unbound type variable " ++ a
  Just t -> do
    -- the substitution should be applied before renaming
    -- (a variable v can be renamed into v0 while the
    -- substitution turns it into bool (which won't be
    -- renamed).
    t' <- substitute t
    fresh t' ng

