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

-- Creates a unique type variable from a string.
fresh :: MonadState Inferencer m => String -> m Simple
fresh a = do
  n <- gets tiNextId
  modify (\s -> s { tiNextId = n + 1 })
  return (TyVar $ a ++ show n)

-- Given a type, returns the constrained type with all the
-- quantified variables renamed with fresh names.
refresh :: MonadState Inferencer m => Type -> m Constrained
refresh (Type gs c) = do
  gs' <- mapM fresh gs
  return $ subs (fromList $ zip gs gs') c

-- Compose the given substitution with the current substitution.
compose :: MonadState Inferencer m => String -> Substitution -> m ()
compose msg ts = do
   n <- gets tiSubstitution
   modify (\s -> s { tiSubstitution = comp msg ts n })

-- Returns a type using the current substitution.
substitute :: (MonadState Inferencer m, Subs a) => a ->  m a
substitute t = do
  n <- gets tiSubstitution
  return (subs n t)

substitute' :: MonadState Inferencer m => Context ->  m Context
substitute' t = do
  n <- gets tiSubstitution
  return (subs' n t)

note :: MonadWriter [Note] m => String -> m ()
note m = do
  tell [NString m]

recordType :: Int -> Constrained -> Context -> Inf ()
recordType k t g = modify (\s -> s { tiTypings = (k,(t,g)) : tiTypings s })

