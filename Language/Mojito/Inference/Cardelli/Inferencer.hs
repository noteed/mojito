{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Language.Mojito.Inference.Cardelli.Inferencer where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Substitution
import Language.Mojito.Inference.Cardelli.Environment

data Inferencer = Inferencer
  { tiNextId :: Int -- to uniquely name type variables
  , tiTypes :: [Simple] -- the available (declared) types
  , tiSubstitution :: Substitution -- the global substitution
  , tiTypings :: [(Int,Simple)] -- the typings for each key
  }
  deriving Show

inferencer :: Inferencer
inferencer = Inferencer
  { tiNextId = 0
  , tiTypes = []
  , tiSubstitution = idSubstitution
  , tiTypings = []
  }

newtype Inf a = Inf
  { runInf ::
    ErrorT String (WriterT [Note] (StateT Inferencer Identity)) a
  }
  deriving
    (Functor, Monad, MonadState Inferencer,
    MonadError String, MonadWriter [Note])

-- Creates a unique type variable from a string.
rename :: MonadState Inferencer m => String -> m Simple
rename a = do
  n <- gets tiNextId
  modify (\s -> s { tiNextId = n + 1 })
  return (TyVar $ a ++ show n)

-- Compose the given substitution with the current substitution.
compose :: MonadState Inferencer m => Substitution -> m ()
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
fresh :: MonadState Inferencer m => Simple -> [String] -> m Simple
fresh t ng = do
  let gs = gvars t ng
  gs' <- mapM rename gs
  return $ subs (zip gs gs') t

-- Retrieves the type corresponding to an identifier,
-- giving fresh names to its generic variables, and
-- applying the current substitution.
retrieve :: String -> Env -> [String] -> Inf Simple
retrieve a tenv ng = case lookup a tenv of
  Nothing -> error $ "unbound type variable " ++ a
  Just t -> do
    -- the substitution should be applied before renaming
    -- (a variable v can be renamed into v0 while the
    -- substitution turns it into bool (which won't be
    -- renamed).
    t' <- substitute t
    fresh t' ng

data Note = Note

--note :: MonadState Inferencer m => String -> m ()
--note m = modify (\s -> s { notes = m : notes s })
note :: Monad m => String -> m ()
note _ = return ()

