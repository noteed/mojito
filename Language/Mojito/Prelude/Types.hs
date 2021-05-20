module Language.Mojito.Prelude.Types where

import Language.Mojito.Syntax.Types

-- Some base types

unit :: Simple
unit = TyCon "()"

bool :: Simple
bool = TyCon "Bool"

int32 :: Simple
int32 = TyCon "Int32"

int64 :: Simple
int64 = TyCon "Int64"

flt32 :: Simple
flt32 = TyCon "Flt32"

flt64 :: Simple
flt64 = TyCon "Flt64"

string :: Simple
string = TyCon "String"

fun :: Simple -> Simple -> Simple
fun a b = TyCon "->" `TyApp` a `TyApp` b

-- container / compound types

wrap :: Simple -> Simple
wrap a = TyCon "Wrap" `TyApp` a

pair :: Simple -> Simple -> Simple
pair a b = TyCon "," `TyApp` a `TyApp` b

tuple3 :: Simple -> Simple -> Simple -> Simple
tuple3 a b c = TyCon ",," `TyApp` a `TyApp` b `TyApp` c

tuple4 :: Simple -> Simple -> Simple -> Simple -> Simple
tuple4 a b c d = TyCon ",," `TyApp` a `TyApp` b `TyApp` c `TyApp` d

list :: Simple -> Simple
list a = TyCon "List" `TyApp` a

tree :: Simple -> Simple
tree a = TyCon "Tree" `TyApp` a

vec2 :: Simple -> Simple
vec2 a = TyCon "Vec2" `TyApp` a

vec3 :: Simple -> Simple
vec3 a = TyCon "Vec3" `TyApp` a

vec4 :: Simple -> Simple
vec4 a = TyCon "Vec4" `TyApp` a

mat3x3 :: Simple -> Simple
mat3x3 a = TyCon "Mat3x3" `TyApp` a

mat4x4 :: Simple -> Simple
mat4x4 a = TyCon "Mat4x4" `TyApp` a
