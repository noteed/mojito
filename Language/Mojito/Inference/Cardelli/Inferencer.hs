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
fresh :: MonadState Inferencer m => String -> m Simple
fresh a = do
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
refresh :: MonadState Inferencer m => Simple -> [String] -> m Simple
refresh t ng = do
  let gs = gvars t ng
  gs' <- mapM fresh gs
  return $ subs (fromList $ zip gs gs') t

data Note = Note

--note :: MonadState Inferencer m => String -> m ()
--note m = modify (\s -> s { notes = m : notes s })
note :: Monad m => String -> m ()
note _ = return ()

