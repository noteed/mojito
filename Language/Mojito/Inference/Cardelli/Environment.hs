{-# LANGUAGE FlexibleContexts #-}
module Language.Mojito.Inference.Cardelli.Environment where

import Data.List ((\\))
import Control.Monad.State

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.Substitution
import qualified Language.Mojito.Inference.Unification as U

-- A type environment maps type variables to types.
-- An association list should be enough but to allow
-- numbers to be treated like identifiers, a function
-- is used when the identifer can't be found in the
-- association list. (The function is provided in the
-- S).
type Env = [(String,Simple)]

-- Returns the generic variables of a type, i.e. the
-- variables not in the list of non-generic variables.
gvars :: Simple -> [String] -> [String]
gvars t ng = vars t \\ ng

-- Extends a type environment with a new pair
-- identifier/type.
extend :: String -> Simple -> Env -> Env
extend s t = (:) (s,t)

