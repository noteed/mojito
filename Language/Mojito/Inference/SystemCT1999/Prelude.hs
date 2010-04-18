module Language.Mojito.Inference.SystemCT1999.Prelude where

import Control.Arrow (second)

import Language.Mojito.Syntax.Expr
import Language.Mojito.Inference.SystemCT1999.Context
import Language.Mojito.Inference.SystemCT1999.Substitution (quantified)
import Language.Mojito.Prelude.Types
import Language.Mojito.Interpretation.Values

someTypes :: [Simple]
someTypes =
  [ unit, bool, int32, flt32, string
  , wrap (TyVar "a")
  , TyVar "a" `fun` TyVar "b"
  , TyVar "a" `pair` TyVar "b"
  , vec2 (TyVar "a"), vec3 (TyVar "a"), vec4 (TyVar "a")
  ]

someContext :: Context
someContext = Context $ map (second quantified . fst) someEnvironment

someEnvironment :: Env
someEnvironment =
  [ (("True", bool), Bool True)
  , (("False", bool), Bool False)
  , (("pi", flt32), Flt32 pi)

  , (("+", int32 `fun` (int32 `fun` int32)), Primitive AddInt32 [])
  , (("-", int32 `fun` (int32 `fun` int32)), Primitive SubInt32 [])
  , (("/", int32 `fun` (int32 `fun` int32)), Primitive DivInt32 [])
  , (("*", int32 `fun` (int32 `fun` int32)), Primitive MulInt32 [])
  , (("negate", int32 `fun` int32), Primitive NegateInt32 [])

  , (("+", flt32 `fun` (flt32 `fun` flt32)), Primitive AddFlt32 [])
  , (("-", flt32 `fun` (flt32 `fun` flt32)), Primitive SubFlt32 [])
  , (("/", flt32 `fun` (flt32 `fun` flt32)), Primitive DivFlt32 [])
  , (("*", flt32 `fun` (flt32 `fun` flt32)), Primitive MulFlt32 [])
  , (("negate", flt32 `fun` flt32), Primitive NegateFlt32 [])

  , (("<", int32 `fun` (int32 `fun` bool)), Primitive LtInt32 [])

  , (("floor", flt32 `fun` int32), Primitive Floor32 [])
  , (("int2flt", int32 `fun` flt32), Primitive Int2Flt32 [])

  , (("cos", flt32 `fun` flt32), Primitive Cos32 [])
  , (("sin", flt32 `fun` flt32), Primitive Sin32 [])
  , (("tan", flt32 `fun` flt32), Primitive Tan32 [])

  , (("Wrap", TyVar "a" `fun` wrap (TyVar "a")), Constructor "Wrap" [TyVar "a"] [])
  , ((",", TyVar "a" `fun` (TyVar "b"  `fun` pair (TyVar "a") (TyVar "b"))), Constructor "," [TyVar "a", TyVar "b"] [])
  , ((",,", TyVar "a" `fun` (TyVar "b"  `fun` (TyVar "c" `fun` tuple3 (TyVar "a") (TyVar "b") (TyVar "c")))), Constructor ",," [TyVar "a", TyVar "b", TyVar "c"] [])
  , ((",,,", TyVar "a" `fun` (TyVar "b"  `fun` (TyVar "c" `fun` (TyVar "d" `fun` tuple4 (TyVar "a") (TyVar "b") (TyVar "c") (TyVar "d"))))), Constructor ",,," [TyVar "a", TyVar "b", TyVar "c", TyVar "d"] [])
  , (("Vec2", TyVar "a" `fun` (TyVar "a" `fun` vec2 (TyVar "a"))), Constructor "Vec2" [TyVar "a", TyVar "a"] [])
  , (("Vec3", TyVar "a" `fun` (TyVar "a" `fun` (TyVar "a" `fun` vec3 (TyVar "a")))), Constructor "Vec3" [TyVar "a", TyVar "a", TyVar "a"] [])
  , (("Vec4", TyVar "a" `fun` (TyVar "a" `fun` (TyVar "a" `fun` (TyVar "a" `fun` vec4 (TyVar "a"))))), Constructor "Vec4" [TyVar "a", TyVar "a", TyVar "a", TyVar "a"] [])
  , (("Mat3x3", foldr fun (mat3x3 $ TyVar "a") (replicate 9 $ TyVar "a")), Constructor "Mat3x3" (replicate 9 $ TyVar "a") [])
  , (("Mat4x4", foldr fun (mat4x4 $ TyVar "a") (replicate 16 $ TyVar "a")), Constructor "Mat4x4" (replicate 16 $ TyVar "a") [])
  ]


