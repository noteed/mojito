{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Language.Mojito.Inference.SystemCT1999.Inferencer where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Context
import Language.Mojito.Inference.Substitution
import Language.Mojito.Inference.SystemCT1999.Note

data Inferencer = Inferencer
  { tiNextId :: Int -- to uniquely name type variables
  , tiTypes :: [Simple] -- the available (declared) types
  , tiSubstitution :: Substitution -- the global substitution
  , tiTypings :: [(Int,(Constrained,Context))] -- the typings for each key
  }

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

-- Creates a unique type or term variable from a string.
rename :: MonadState Inferencer m => String -> m String
rename a = do
  n <- gets tiNextId
  modify (\s -> s { tiNextId = n + 1 })
  return (a ++ show n)

-- Given a type, returns the constrained type with all the
-- quantified variables renamed with fresh names.
freshQuantified :: MonadState Inferencer m => Type -> m Constrained
freshQuantified (Type gs c) = do
  gs' <- mapM (liftM TyVar . rename) gs
  return $ subs (fromList $ zip gs gs') c

