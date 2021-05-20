{-# LANGUAGE FlexibleContexts #-}
module Language.Mojito.Inference.Cardelli.Prelude where

import Language.Mojito.Syntax.Types
import Language.Mojito.Prelude.Types
import Language.Mojito.Inference.Cardelli.Environment

-- TODO should be called someContext to match naming
-- used in SystemCT 1999
someEnvironment :: Env
someEnvironment =
  [ ("true", bool)
  , ("false", bool)
  , ("1", int32)
  , ("2", int32)
  , ("mkPair", fun (TyVar "a") (fun (TyVar "b") (pair (TyVar "a") (TyVar "b"))))
  , ("fst", fun (pair (TyVar "a") (TyVar "b")) (TyVar "a"))
  , ("snd", fun (pair (TyVar "a") (TyVar "b")) (TyVar "b"))
  , ("iszero", fun int32 bool)
  , ("+", fun int32 (fun int32 int32))
  ]
