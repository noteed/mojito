module Language.Mojito.Interpretation.Values where

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types

type Env = [((String, Simple), Val)]

data Primitive =
    AddInt32
  | SubInt32
  | DivInt32
  | MulInt32
  | NegateInt32
  | AddFlt32
  | SubFlt32
  | DivFlt32
  | MulFlt32
  | NegateFlt32
  | LtInt32
  | Floor32
  | Int2Flt32
  | Cos32
  | Sin32
  | Tan32
  deriving Show

data Val =
    Unit
  | Int32 Int
  | Flt32 Float
  | Bool Bool
  | String String
  | Data String [Val]
  | Closure String Simple (Expr Simple) Env
  | Primitive Primitive [Val]
  | Constructor String [Simple] [Val]
  deriving Show

instance Eq Val where
  Unit == Unit = True
  Int32 a == Int32 b = a == b
  Flt32 a == Flt32 b = a == b
  Data c1 v1 == Data c2 v2 = c1 == c2 && v1 == v2
  _ == _ = error "TODO Eq Val"

----------------------------------------------------------------------
-- Build Val for literals
----------------------------------------------------------------------

-- TODO check the type and magnitude

fltLit :: Simple -> Double -> Val
fltLit _ v = Flt32 $ realToFrac v

intLit :: Simple -> Integer -> Val
intLit _ v = Int32 $ fromInteger v

strLit :: Simple -> String -> Val
strLit _ v = String v
