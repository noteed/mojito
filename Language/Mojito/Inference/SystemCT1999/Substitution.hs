module Language.Mojito.Inference.SystemCT1999.Substitution where

import Data.List (intersect, nub, union, (\\))

import Language.Mojito.Syntax.Types
import Language.Mojito.Inference.SystemCT1999.Context

----------------------------------------------------------------------
-- Substitution
----------------------------------------------------------------------

close :: Constrained -> Context -> Type
close k g = Type as k
  where as = tv k \\ tv' g

-- Returns the list of type variables in a simple type.
vars :: Simple -> [String]
vars (TyVar a) = [a]
vars (TyCon _) = []
vars (TyApp a b) = vars a `union` vars b

-- Returns the free type variables of a type, simple type, constraint,
-- list of constraints.
class TV a where tv :: a -> [String]

instance TV a => TV [a] where
  tv ts = nub $ concatMap tv ts

instance TV Simple where
  tv = vars

instance TV Constraint where
  tv (Constraint _ t) = vars t

instance TV Constrained where
  tv (Constrained cs t) = tv cs `union` vars t

instance TV Type where
  tv (Type as c) = tv c \\ as

-- Returns the free type variables of a typing context.
tv' :: Context -> [String]
tv' g = nub $ concatMap (tv . snd) (ctxAssoc g)

quantified :: Simple -> Type
quantified t = Type (tv t) $ Constrained [] t

-- A (type) substitution is a function from type variables to
-- simple types or type constructor that differs from the identity
-- function only on finitely
-- many variable (definition from Camarao 1999, Type Inference for
-- Overloading without Restrictions, Declarations or Annotations).
-- It is represented by an association list. Applying the
-- substitution to a type is done by looking up the type from the
-- association-list. If it is not found, it is returned as-is.
-- A substitution can also be applied to a type environment.
-- Instead of modifying the environment each time a new substitution
-- is computed, the substitution is applied when an identifer is
-- looked up (see the function 'retrieve').
type Substitution = [(String,Simple)]

showSubstitution :: Substitution -> String
showSubstitution = unlines . map (\(a,b) -> a ++ " -> " ++ showSimple b)

idSubstitution :: Substitution
idSubstitution = []

fromList :: [(String,Simple)] -> Substitution
fromList = id

-- Builds a substitution which replace the first argument
-- by the second.
replace :: String -> Simple -> Substitution
replace a t = [(a,t)]

-- The \dag LaTeX symbol, which 'shadows' a substitution by
-- another. Here, it is implemented by the fact that a lookup
-- in a association-list will return the first match.
dag :: [(String, (Simple,Simple))] -> [(String, (Simple,Simple))] -> [(String, (Simple,Simple))]
dag s1 s2 = s2 ++ s1

class Subs a where subs :: Substitution -> a -> a

instance Subs a => Subs [a] where
  subs s = map (subs s)

instance (Subs a, Subs b) => Subs (a,b) where
  subs s (a,b) = (subs s a, subs s b)

-- Apply a substitution to a simple type.
instance Subs Simple where
  subs [] t = t
  subs s t = case t of
    TyVar a -> maybe t id $ lookup a s
    TyCon _ -> t
    TyApp a b -> subs s a `TyApp` subs s b

instance Subs Constraint where
  subs [] c = c
  subs s (Constraint o t) = Constraint o (subs s t)

instance Subs Constrained where
  subs [] c = c
  subs s (Constrained cs t) = Constrained (subs s cs) (subs s t)

instance Subs Type where
  subs [] t = t
  subs s (Type as c) =
    let s' = filter f s
        f = not . (`elem` as) . fst
    in Type as (subs s' c)

subs' :: Substitution -> Context -> Context
subs' s g = Context (map (\(a,b) -> (a, s `subs` b)) $ ctxAssoc g)

comp :: String -> Substitution -> Substitution -> Substitution
comp msg ts1 ts2 = foldr f ts2 ts1
  where f t ts = ext (fst t) (snd t) ts

        -- adds a new pair to the substitution, and "performs" it in-place.
        ext :: String -> Simple -> [(String,Simple)] -> [(String,Simple)]
        ext t1 t2 ts = case lookup t1 ts of
          Nothing -> case t2 of
                       TyCon _ -> (t1,t2) : rep t1 t2 ts
                       TyApp _ _ -> (t1,t2) : rep t1 t2 ts
                       TyVar a -> case lookup a ts of
                                    Nothing -> (t1,t2) : rep t1 t2 ts
                                    Just t3 -> (t1,t3) : rep t1 t3 ts
          Just _ -> error $ "comp: " ++ show t1 ++ " is already present in : " ++ show ts ++ " to be replaced by " ++ show t2 ++ " -- " ++ msg
        rep a b cs = let g (x,y) = (x, [(a,b)] `subs` y)
                     in map g cs

intersectSubs :: [Substitution] -> Substitution
intersectSubs [] = error $ "intersectSubs called on empty set"
intersectSubs [s] = s
intersectSubs (s:bigs) = s `restrict` v
  where v = [fst a | a <- s, b <- s', a == b]
        s' = intersectSubs bigs

-- TODO QuickCheck Property: the application of a substitution S to s
-- (si stands for sigma) is capture-free if tv(Ss) == tv(S(tv(s))).

----------------------------------------------------------------------
-- Restriction
----------------------------------------------------------------------

-- Restriction of a substitution to a set of type variables.
restrict :: Substitution -> [String] -> Substitution
restrict s vs = filter f s
  where f = (`elem` vs) . fst

-- Restriction of a set of constraints to a set of type variables.
restr :: [Constraint] -> [String] -> [Constraint]
restr [] _ = []
restr [Constraint o t] vs | tv t `intersect` vs == [] = []
                          | otherwise = [Constraint o t]
restr (c:cs) vs = restr [c] vs `union` restr cs vs

-- Closure of restricting a set of constraints to a set of
-- type variables.
restr' :: [Constraint] -> [String] -> [Constraint]
restr' k vs | null (v \\ vs )= k'
            | otherwise = restr' k v
  where k' = k `restr` vs
        v = tv k'

restrictCtx :: Context -> String -> Context
restrictCtx (Context xs) s = Context $ filter ((== s) . fst) xs

