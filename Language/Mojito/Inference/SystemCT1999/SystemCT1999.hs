{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- SystemCT
--   from Camarao 1999,
--   Type Inference for Overloading without
--   Restrictions, Declarations or Annotations
--
-- Based on Cardelli.hs.

module Language.Mojito.Inference.SystemCT1999.SystemCT1999 where

import Data.List (groupBy, union, (\\))
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Language.Mojito.Syntax.Expr
import Language.Mojito.Syntax.Types
import Language.Mojito.Syntax.ExprBuilder
import Language.Mojito.Prelude.Types

import Language.Mojito.Inference.Context
import Language.Mojito.Inference.Substitution
import Language.Mojito.Inference.Unification
import Language.Mojito.Inference.SystemCT1999.Note
import Language.Mojito.Inference.SystemCT1999.Inferencer
import Language.Mojito.Inference.SystemCT1999.LCG

----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------

-- Infers the typing of an expression in a context. This returns some logs, the state
-- of the inference and either the typing or an error.
infer :: [Simple] -> Expr Int -> Context -> ((Either String (Constrained,Context), [Note]), Inferencer)
infer ts e g = runIdentity $ runStateT (runWriterT $ runErrorT $ runInf $ pp e g) (inferencer { tiTypes = ts })

infer' :: [Simple] -> Expr Int -> Context -> ((Either String (Expr Simple), [Note]), Inferencer)
infer' ts e g = runIdentity $ runStateT (runWriterT $ runErrorT $ runInf $ go) (inferencer { tiTypes = ts })
  where go = do (c,g') <- pp e g
                case kgs c g' of
                  [] -> throwError "no type."
                  [t] -> do
                    ty <- gets tiTypings
                    s <- gets tiSubstitution
                    return $ duplicate ty s e t
                  _ -> throwError "more than one type."

readInferedExpr :: [Simple] -> Context -> String -> Expr Simple
readInferedExpr ts g str = case readExpr str of
  Left err -> error err
  Right e ->
    case infer' ts e g of
    ((Left err,_),_) -> error err
    ((Right e',_),_) -> e'

makeDefaultTypes :: Substitution -> [(Int,(Constrained,Context))] -> [(Int,Simple)]
makeDefaultTypes s ts = map f ts
  where f (k,(t,g)) = let (Constrained k' t',g') = (s `subs` t,s `subs'` g)
                          bigs = satc k' g'
                      in (k, h $ map (`subs` t') bigs)
        h [] = error "no type"
        h [t] = t
        h tt = error $ "TODO implement some default type mechanism: " ++ concatMap showSimple tt

giveTypes :: Expr Int -> Substitution -> [(Int,(Constrained,Context))] -> Expr [Simple]
giveTypes e s ts = fmap f e
  where f k = case lookup k ts of
                Nothing -> error $ "no typing for node " ++ show k
                Just (t,g) -> let (Constrained k' t',g') = (s `subs` t,s `subs'` g)
                                  bigs = satc k' g'
                              in map (`subs` t') bigs

giveTypes' :: Expr Int -> Substitution -> [(Int, (Constrained, Context))] -> Expr [Simple]
giveTypes' e s ts = fmap l' e
  where
      (ts',ks) = unzip $ map f ts -- mapping i/simple and mapping simple/(constrains,context)
      f (i,(t,g)) = let Constrained k' t' = s `subs` t
                        g' = s `subs'` g
                    in ((i,t'),(t',(k',g')))
      ks' :: [[(Simple,([Constraint],Context))]]
      ks' = groupBy ((==) `on` fst) ks -- grouping simple/(constraints,context)
      h :: [(Simple,([Constraint],Context))] -> (Simple,[Substitution])
      h xs@((t',_):_) = (t', grp $ unzip $ map snd xs)
      h _ = error "unexpected"
      grp (uk,ug) = let ug' = unionContexts ug
                        uk' = foldl union [] uk
                    in satc uk' ug'
      ss = map h ks'
      l i = do t <- lookup i ts'
               bigs <- lookup t ss
               return $ map (`subs` t) bigs
      l' i = case l i of
               Nothing -> error "giveTypes'"
               Just r -> r

inferTypes :: [Simple] -> Context -> String -> Expr [Simple]
inferTypes ts g str = case readExpr str of
  Left err -> error err
  Right e ->
    case infer ts e g of
    ((Left err,_),_) -> error err
    ((Right _,_),i) -> giveTypes e (tiSubstitution i) (tiTypings i)

inferTypes' :: [Simple] -> Context -> String -> Expr [Simple]
inferTypes' ts g str = case readExpr str of
  Left err -> error err
  Right e ->
    case infer ts e g of
    ((Left err,_),_) -> error err
    ((Right _,_),i) -> giveTypes' e (tiSubstitution i) (tiTypings i)

giveDefaultTypes :: Expr [Simple] -> Expr Simple
giveDefaultTypes e = fmap f e
  where f [] = error "no type"
        f [t] = t
        f tt = error $ "TODO implement some default type mechanism: " ++ concatMap showSimple tt

----------------------------------------------------------------------
-- system-ct type-inference specific helper functions
----------------------------------------------------------------------

ambiguous :: [String] -> [Constraint] -> [String] -> Bool
ambiguous v1 k v = not (null v') && all (`elem` v1) v' 
  where v' = tv k \\ tv (restr' k v)

unresolved :: [Constraint] -> Context -> [Constraint]
unresolved [] _ = []
unresolved (Constraint o t : k) g = k' `union` unresolved k g
  where k' = case satc [Constraint o t] g of
               [s] -> unresolved (subs s k) g
               _ -> [Constraint o t]

rhoc :: MonadError String m => Type -> [Type] -> m ()
rhoc _ [] = return ()
rhoc s ss = do
  let t = simple s
      checkClosed s' = unless (null $ tv s') $ throwError $
        "the previous type " ++ show s' ++ " is not closed."
      checkUnification s' = unless (isLeft $ unify t $ simple s') $ throwError $
        "the new type " ++ show s ++ " unifies with previous type " ++ show s' ++ "."
      checkAll s' = checkClosed s' >> checkUnification s'
  unless (null ss) $ do
    unless (null $ tv s) $ throwError $ "the new type " ++ show s ++ " is not closed."
    mapM_ checkAll ss

satc :: [Constraint] -> Context -> [Substitution]
satc [] _ = [idSubstitution]
satc [Constraint _ t] _ | null (tv t) = [idSubstitution] -- FIXME, not in the paper
satc [Constraint o t] g = mapMaybe f kt
  where f (ki,ti) = case unify t ti of
                      Right s ->
                       if not . null $ satc (s `subs` ki) g
                       then Just s else Nothing
                      Left _ -> Nothing
        kt = map (\(Type _ (Constrained k t')) -> (k,t')) (g `types` o)
satc (k:ks) g =
  [comp "satc" sij si | si <- satc [k] g, sij <- satc (si `subs` ks) g]

-- [[("a",TyCon "int32"),("b",TyCon "flt32")],
--  [("a",TyCon "flt32"),("b",TyCon "int32")]]
satcTest1 :: [Substitution]
satcTest1 = satc ks g
  where g = Context [("f", Type [] $ Constrained [] $ int32 `fun` flt32),
             ("f", Type [] $ Constrained [] $ flt32 `fun` int32),
             ("x", Type [] $ Constrained [] $ int32),
             ("x", Type [] $ Constrained [] $ flt32)]
        ks = [Constraint "f" $ TyVar "a" `fun` TyVar "b",
              Constraint "x" $ TyVar "a"]

-- Given a constrained type and a context, returns the possible simple types.
-- FIXME this should make sure all constraints are satisfied ?
kgs :: Constrained -> Context -> [Simple]
kgs (Constrained k t) g = map (`subs` t) (satc k g)

kgs' :: [(Int,(Constrained,Context))] -> Substitution
  -> Int -> [Simple]
kgs' ty s k =
  case lookup k ty of
    Nothing -> error "kgs': Should not happen."
    Just (k1,g1) -> kgs (s `subs` k1) (s `subs'` g1)

----------------------------------------------------------------------
-- 'duplicate' turns each implicitely overloaded symbols into a
-- definition for each of its types.
----------------------------------------------------------------------

duplicate :: [(Int, (Constrained, Context))] -> Substitution
  -> Expr Int -> Simple -> Expr Simple
duplicate ty s_ e t = case e of
  Id _ x -> Id t x
  FltLit _ l -> FltLit t l
  IntLit _ l -> IntLit t l
  StrLit _ l -> StrLit t l
  Let _ (Def o e1) e2 ->
    let e2' = duplicate ty s e2 t
        kgs_ = kgs' ty s (unExpr e1)
    in case kgs_ of
         [] -> error "no type."
         [t'] -> let e1' = duplicate ty s e1 t'
                 in Let t (Def o e1') e2'
         ts -> let e1s = map (duplicate ty s e1) ts
               in duplicateLet t o e1s e2'
  Let _ _ _ -> error "unhandeled Let expression"
  Case _ e1 alts ->
    let kgs_ = kgs' ty s (unExpr e1)
    in case kgs_ of
         [] -> error "no type."
         [t'] -> let alts' = map (duplicateAlt ty s t' t) alts
                     e1' = duplicate ty s e1 t'
                 in Case t e1' alts'
         _ -> error "more than one type."
  Fun _ u e1 -> case t of
    TyApp (TyCon "->" `TyApp` _) t1 ->
      let e1' = duplicate ty s e1 t1
      in Fun t u e1'
    _ -> error "not a fun type."
  App _ e1 e2 ->
    -- discover t2 for which e1 has type t2 -> t
    let kgs_ = kgs' ty s (unExpr e1)
    in case kgs_ of
         [] -> error "no type."
         ts -> let t2 = domForType t ts
                   e1'= duplicate ty s e1 (t2 `fun` t)
                   e2' = duplicate ty s e2 t2
               in App t e1' e2'
  HasType _ e1 t' -> duplicate ty s e1 t'
  If _ e1 e2 e3 ->
    let e1' = duplicate ty s e1 bool
        e2' = duplicate ty s e2 t
        e3' = duplicate ty s e3 t
    in If t e1' e2' e3'
  where s = case lookup (unExpr e) ty of
              Nothing -> error "duplicate: Should not happen."
              Just (Constrained _ t1_, _) ->
                case unify (s_ `subs` t1_) t of
                  Left err -> error $ "duplicate: Should not happen: " ++ err
                  Right s' -> comp "duplicate" s' s_

duplicateLet :: k -> String -> [Expr k] -> Expr k -> Expr k
duplicateLet _ _ [] e2 = e2
duplicateLet t o (e1:es) e2 = Let t (Def o e1) $ duplicateLet t o es e2

duplicateAlt :: [(Int,(Constrained,Context))] -> Substitution
  -> Simple -> Simple -> Alternative Int -> Alternative Simple
duplicateAlt ty s t1 t2 (Alternative p e) =
  let p' = duplicatePat ty s p t1
      e' = duplicate ty s e t2
  in Alternative p' e'

duplicatePat :: [(Int,(Constrained,Context))] -> Substitution
  -> Pattern Int -> Simple -> Pattern Simple
duplicatePat _ _ (PatVar _ v) t = PatVar t v
duplicatePat _ _ (PatCon _ _ c []) t = PatCon t t c []
duplicatePat ty s (PatCon _ k c ps) t@(t' `TyApp` _) =
  let [ts] = kgs' ty s k
      ps' = map (uncurry $ duplicatePat ty s) $ zip ps (init $ fargs ts)
  in PatCon t t' c ps'
duplicatePat _ _ p t = error $ "duplicatePat: unexpected " ++ show p ++ ", " ++ showSimple t

domForType :: Simple -> [Simple] -> Simple
domForType t [] = error $ "no type has the form _ -> " ++ showSimple t ++ "."
domForType t ((TyApp (TyCon "->" `TyApp` t2) t'):_) | t == t' = t2
domForType t ((TyApp (TyCon "->" `TyApp` t2) t'):ts) = case unify t t' of
  Right s -> s `subs` t2
  Left _ -> domForType t ts

----------------------------------------------------------------------
--
----------------------------------------------------------------------


note :: String -> Inf ()
note m = do
  tell [NString m]

recordType :: Int -> Constrained -> Context -> Inf ()
recordType k t g = modify (\s -> s { tiTypings = (k,(t,g)) : tiTypings s })

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

-- Checks if the given type is available.
isTypeInScope :: MonadState Inferencer m => Simple -> m Bool
isTypeInScope t = do
  let vs = tv t
  vs' <- mapM (liftM TyVar . rename) (tv t)
  let t' = subs (fromList $ zip vs vs') t
  ts <- gets tiTypes
  return $ (not . null $ filter (isRight . unify t') ts)
           && all (`elem` tc' ts) (tc t)

-- Checks if the given value constructor is available.
isConstructorInScope :: MonadState Inferencer m => String -> m Bool
isConstructorInScope _ = do
  error "TODO isConstructorInScope"

----------------------------------------------------------------------
--
----------------------------------------------------------------------

-- Not really pt:
-- - If no type for x in g, then a new type variable is returned
--   with a singleton context.
-- - When there is one type, the quantified variables are renamed.
pt :: String -> Context -> Inf (Constrained,Context)
pt x g = do
  case g `types` x of
    [] -> do
      a <- rename "a"
      let a' = Constrained [] (TyVar a)
          ret = (a', Context [(x, Type [] $ a')])
--      note $ "pt " ++ x ++ showContext g ++ " = " ++ (\(i,j) -> showConstrained i ++ showContext j) ret
      return ret
    [t] -> do
      c <- freshQuantified t
      let ret = (c, g)
--      note $ "pt " ++ x ++ showContext g ++ " = " ++ (\(i,j) -> showConstrained i ++ showContext j) ret
      return ret
    ti -> do
      x' <- rename "b"
      let g' = tsubs x x' g
      t <- lcg ti
      let ret = (Constrained [Constraint x' t] t, g')
--      note $ "pt " ++ x ++ showContext g ++ " = " ++ (\(i,j) -> showConstrained i ++ showContext j) ret
      return ret

-- term variable substitution on contexts
tsubs :: String -> String -> Context -> Context
tsubs x x' gs = Context [if a == x then (x',b) else (a,b) | (a,b) <- ctxAssoc gs]

----------------------------------------------------------------------
-- Type inference
----------------------------------------------------------------------

pp :: Expr Int -> Context -> Inf (Constrained,Context)
pp e g =
  (\(a,b) -> do a' <- substitute a
                b' <- substitute' b
                return (a',b'))
  =<< case e of
  Id key x -> do
    (t1, g1) <- pt x g
    recordType key t1 g1
    tell [NId key x g (t1, g1)]
    return (t1, g1)

  FltLit key x -> do
    (t1, g1) <- do c <- freshQuantified $ typed flt32 -- mimic what pt does
                   return (c, g)
    recordType key t1 g1
    tell [NId key (show x) g (t1, g1)] -- TODO not an Id
    return (t1, g1)

  IntLit key x -> do
    (t1, g1) <- do c <- freshQuantified $ typed int32 -- mimic what pt does
                   return (c, g)
    recordType key t1 g1
    tell [NId key (show x) g (t1, g1)] -- TODO not an Id
    return (t1, g1)

  StrLit key x -> do
    (t1, g1) <- do c <- freshQuantified $ typed string -- mimic what pt does
                   return (c, g)
    recordType key t1 g1
    tell [NId key (show x) g (t1, g1)] -- TODO not an Id
    return (t1, g1)

  Let key (Def o e1) e2 -> do
    (c1_, g1_) <- pp e1 g
    gext <- letExtend g o (close c1_ g)
    (Constrained k2 t2, g2) <- pp e2 (unionContexts [gext, g1_]) -- Adding g1 is not in the type rules but I believe it should
    g1 <- substitute' g1_
    Constrained k1 _ <- substitute c1_

    let c = combine "Let" g (k1 `union` k2) (unionContexts [g1, g2]) t2
    case c of
      Nothing -> do tell [NLet key o e1 e2 g Nothing]
                    throwError $ unlines [
                      "k1 and k2 cannot be satisfied in g1 and g2",
                      "k1:",
                      showConstraints k1,
                      "k2:",
                      showConstraints k2,
                      "g1:",
                      showContext g1,
                      "g2:",
                      showContext g2
                      ]
      Just (ret, g', sg) -> do compose "combine:" sg
                               recordType key ret g'
                               tell [NLet key o e1 e2 g (Just (ret,g'))]
                               return (ret, g')

  Let _ _ _ -> do error "Unhandeled Let expression"

-- to type
--   case e1 of
--     pi -> ei
-- 1. type e1
-- 2. type pi -> ei
-- like lambdas where each variable in the pattern is introduced,
-- type the pi like applications,
-- type the ei,
-- unify e1 and the pi,
-- unify the ei
-- or : type pi -> ei like a lambda with multiple vars and give it
-- a single funtion type ti and unify the ti.
  Case key e1 alts -> do
    (c1_, g1_) <- pp e1 g
    cgs_ <- mapM (ppAlt g) alts
    Constrained k1_ t1_ <- substitute c1_
    cgs' <- mapM (\(a,b) -> do { a' <- substitute a ; b' <- substitute' b ; return (a',b') }) cgs_
    a <- rename "l"
    s <- unify' (zip (repeat $ t1_ `fun` TyVar a) $ map (smpl . fst) cgs')
    compose "Case" s
    cgs <- mapM (\(a_,b) -> do { a' <- substitute a_ ; b' <- substitute' b ; return (a',b') }) cgs'
    g1 <- substitute' g1_
    k1 <- substitute k1_
    ty <- substitute (TyVar a)
    let uk = foldl (\b (c,_) -> b `union` cstr c) [] cgs
        ug = Context $ foldl (\ c (_,b) -> c `union` ctxAssoc b) [] cgs
    let c = combine "Case" g (k1 `union` uk) (unionContexts [g1, ug]) ty
    case c of
      Nothing -> do throwError "uk cannot be satisfied in ug"
      Just (ret, g', sg) -> do compose "combine:" sg
                               recordType key ret g'
                               return (ret, g')

{-
  Fun key u e1 -> do
    (Constrained k t, g'_) <- pp e1 g
    case lookup u $ ctxAssoc g'_ of
      Just (Type [] (Constrained [] t')) -> do
        let ret = Constrained k (t' `fun` t)
            g' = Context $ filter (/= (u,Type [] (Constrained [] t'))) (ctxAssoc g'_)
        recordType key ret g'
        tell [NFun key u e1 g (ret,g')]
        return (ret, g')
      Nothing -> do
        a <- TyVar `fmap` rename "i"
        let ret = Constrained k (a `fun` t)
        recordType key ret g'_
        tell [NFun key u e1 g (ret,g'_)]
        return (ret, g'_)
      _ -> error "unexpected type"
-}

  Fun key u e1 -> do
    t' <- fmap TyVar $ rename "i"
    let gext = lamExtend g u (typed t')
    (Constrained k t, g'_) <- pp e1 gext
    t'' <- substitute t'
    let ret = Constrained k (t'' `fun` t)
        g' = Context $ filter ((/= u) . fst) (ctxAssoc g'_)
    recordType key ret g'
    tell [NFun key u e1 g (ret,g')]
    return (ret, g')

  App key e1 e2 -> do
    (c1_, g1_) <- pp e1 g
    (Constrained k2_ t2, g2_) <- pp e2 g
    Constrained k1_ t1_ <- substitute c1_
    a <- rename "j"
--    note $ "_unify " ++ showSimple t1 ++ " " ++ showSimple (t2 `fun` TyVar a) ++ " " ++ show (tv' g)
--    let s = case _unify t1 (t2 `fun` TyVar a) (tv' g) of
--    FIXME This should use _unify ('Unify' in the paper) but I don't know exactly what it is.
    s <- unify t1_ (t2 `fun` TyVar a)
    compose "App" s
    k1 <- substitute k1_
    g1 <- substitute' g1_
    k2 <- substitute k2_
    g2 <- substitute' g2_
    ty <- substitute (TyVar a)

    let c = combine "App2" g (k1 `union` k2) (unionContexts [g1, g2]) ty
    case c of
      Nothing -> do tell [NApp key e1 e2 g Nothing]
                    throwError $ unlines [
                      "k1 and k2 cannot be satisfied in g1 and g2",
                      "k1:",
                      showConstraints k1,
                      "k2:",
                      showConstraints k2,
                      "g1:",
                      showContext g1,
                      "g2:",
                      showContext g2
                      ]
      Just (ret, g', sg) -> do compose "combine:" sg
                               recordType key ret g'
                               tell [NApp key e1 e2 g (Just (ret,g'))]
                               return (ret, g')

  HasType key e1 t -> do
    b <- isTypeInScope t
    when (not b) $ throwError $ "the type " ++ showSimple t ++ " is not in scope."
    (Constrained k1_ t1_, g1_) <- pp e1 g
    s <- unify t1_ t
    compose "HasType" s
    g1 <- substitute' g1_
    k1 <- substitute k1_

    let c = combine "HasType" g k1 g1 t
    case c of
      Nothing -> do tell [NHasType key e1 t g Nothing]
                    throwError "k1 and k2 cannot be satisfied in g'"
      Just (ret, g', sg) -> do compose "combine:" sg
                               recordType key ret g'
                               tell [NHasType key e1 t g (Just (ret,g'))]
                               return (ret, g')

  If key e1 e2 e3 -> do
    _ <- pp (HasType (-1) e1 bool) g -- FIXME constraints do not participate to the global substitution (same in App and Let)
    (c2_, g2_) <- pp e2 g
    (Constrained k3 t3, g3) <- pp e3 g
    Constrained k2_ t2_ <- substitute c2_
    s <- unify t2_ t3
    compose "If" s

    g2 <- substitute' g2_
    k2 <- substitute k2_

    let c = combine "If" g (k2 `union` k3) (unionContexts [g2, g3]) t3
    case c of
      Nothing -> do tell [NIf key e1 e2 e3 g Nothing]
                    throwError "k1 and k2 cannot be satisfied in g'"
      Just (ret, g', sg) -> do compose "combine:" sg
                               recordType key ret g'
                               tell [NIf key e1 e2 e3 g (Just (ret,g'))]
                               return (ret, g')

ppAlt :: Context -> Alternative Int -> Inf (Constrained,Context)
ppAlt g (Alternative p e) = do
  let pvs = map fst $ patVars p
      pe = patExpr p
  ts <- replicateM (length pvs) (fmap TyVar $ rename "k")
  let gext = foldl lamExtend' g (zip pvs (map typed ts))
  (c1_, g1_) <- pp pe gext
  (Constrained k2 t2, g2) <- pp e gext
  Constrained k1 t1 <- substitute c1_
  g1 <- substitute' g1_
  let c = combine "ppAlt" g (k1 `union` k2) (unionContexts [g1, g2]) (t1 `fun` t2)
  case c of
    Nothing -> do throwError "k1 and k2 cannot be satisfied in g'"
    Just (ret, g', sg) -> do compose "combine:" sg
                             let fg' = Context $ filter (not . (`elem` pvs) . fst) (ctxAssoc g')
                             return (ret, fg')

letExtend :: MonadError String m => Context -> String -> Type -> m Context
letExtend g o t = do
  rhoc t (g `types` o) `catchError` (\e -> throwError $ "Can't overload " ++ o ++ ": " ++ e)
  return $ Context $ ctxAssoc g `union` [(o,t)]

lamExtend :: Context -> String -> Type -> Context
lamExtend g u t = Context $ (u,t) : filter ((/= u) . fst) (ctxAssoc g)

lamExtend' :: Context -> (String, Type) -> Context
lamExtend' g (u,t) = lamExtend g u t


-- Returns Nothing if k1 and k2 cannot be satisfied in g1 and g2.
-- FIXME the g, g' used in unresolved and tv' are not correct I think w.r.t. to
-- the algorithm in the paper.
combine :: String ->
  Context -> [Constraint] -> Context
  -> Simple
  -> Maybe (Constrained, Context, Substitution)
combine msg g uk ug ty = do
  let bigs = satc uk ug
  if null bigs
    then Nothing
    else
      let sg = intersectSubs bigs --`restrict` (tv uk \\ tv' g) -- not in the algorithm
          t = sg `subs` ty
          k = unresolved (sg `subs` uk) ug
      -- FIXME is this what tv(t,g) means ?
          ret = Constrained (k `restr'` (tv t `union` tv' g)) t
      in Just (ret, ug, sg)

----------------------------------------------------------------------
-- Report
----------------------------------------------------------------------

-- maximal type, minimal context, notes
data Report =
  Report
  { rCode :: String
  , rExpr :: Expr Int
  , rInitialContext :: Context
  , rType :: Constrained
  , rContext :: Context
  , rTypings :: [(Int, (Constrained,Context))]
  , rSubstitution :: Substitution
  , rNotes :: [Note]
  }
  | NoReport
  { rCode :: String
  , rError :: String
  , rNotes :: [Note]
  }

report :: [Simple] -> Context -> String -> Report
report ts g str =
  case readExpr str of
    Left err -> NoReport str err []
    Right e ->
      case infer ts e g of
        ((Left err,n),_) -> NoReport str err n
        ((Right (t,c),n),s) ->
          let
            tgs = tiTypings s
            sub = tiSubstitution s
          in Report str e g t c tgs sub n

----------------------------------------------------------------------
-- utility code
----------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight _ = True

