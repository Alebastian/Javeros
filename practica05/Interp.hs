module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]


-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun []     e = Just e
curryFun (x:xs) e = Fun x <$> curryFun xs e

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp f []     = Just f
curryApp f (a:as) = curryApp (App f a) as

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _  []           = Nothing
binaryOp _  [x]          = Just x
binaryOp op (x:y:rest)   = binaryOp op (op x y : rest)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond []             elseExpr = desugar elseExpr
desugarCond ((c, t) : cs)  elseExpr =
  desugar c >>= \c' ->
  desugar t >>= \t' ->
  desugarCond cs elseExpr >>= \e' ->
  Just (If c' t' e')

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x)       = Just (Id x)
desugar (NumS n)      = Just (Num n)
desugar (BooleanS b)  = Just (Boolean b)
desugar (AddS es)     = mapM desugar es >>= binaryOp Add
desugar (SubS es)     = mapM desugar es >>= binaryOp Sub
desugar (NotS e)      = Not <$> desugar e
desugar (LetS x e1 e2) =
  desugar e1 >>= \e1' ->
  desugar e2 >>= \e2' ->
  Just (App (Fun x e2') e1')
desugar (LetStarS bindings body) =
  desugar (foldr (\(x, e) acc -> LetS x e acc) body bindings)
desugar (FunS xs e)   = curryFun xs =<< desugar e
desugar (AppS f args) =
  desugar f >>= \f' ->
  mapM desugar args >>= \args' ->
  curryApp f' args'
desugar (IfS c t e) =
  desugar c >>= \c' ->
  desugar t >>= \t' ->
  desugar e >>= \e' ->
  Just (If c' t' e')
desugar (CondS cs elseExpr) = desugarCond cs elseExpr
desugar (LetRecS f def body) =
  desugar (LetS f (AppS (IdS "Y") (FunS [f] def)) body)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v) : env)
  | x == y    = Just v
  | otherwise = lookupEnv x env

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV e env) = bigStep env e >>= strict
strict v             = Just v

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep _   (Num n)     = Just (NumV n)
bigStep _   (Boolean b) = Just (BooleanV b)
bigStep env (Id x)      = lookupEnv x env
bigStep env (Fun x e)   = Just (ClosureV x e env)
bigStep env (App f a)   =
  bigStep env f >>= strict >>= \vf ->
  applyClosure vf a env
bigStep env (Add e1 e2) =
  bigStep env e1 >>= strict >>= \v1 ->
  bigStep env e2 >>= strict >>= \v2 ->
  addValues v1 v2
bigStep env (Sub e1 e2) =
  bigStep env e1 >>= strict >>= \v1 ->
  bigStep env e2 >>= strict >>= \v2 ->
  subValues v1 v2
bigStep env (Not e)     =
  bigStep env e >>= strict >>= \v ->
  notValue v
bigStep env (If c t e)  =
  bigStep env c >>= strict >>= \vc ->
  ifValue vc t e env

-- Funciones auxiliares para bigStep (sin do ni case)
applyClosure :: Value -> ASA -> Env -> Maybe Value
applyClosure (ClosureV p b envf) a env = bigStep ((p, ExprV a env) : envf) b
applyClosure _ _ _                     = Nothing

addValues :: Value -> Value -> Maybe Value
addValues (NumV n1) (NumV n2) = Just (NumV (n1 + n2))
addValues _ _                 = Nothing

subValues :: Value -> Value -> Maybe Value
subValues (NumV n1) (NumV n2) = Just (NumV (max 0 (n1 - n2)))
subValues _ _                 = Nothing

notValue :: Value -> Maybe Value
notValue (BooleanV b) = Just (BooleanV (not b))
notValue _            = Nothing

ifValue :: Value -> ASA -> ASA -> Env -> Maybe Value
ifValue (BooleanV True)  t _ env = bigStep env t
ifValue (BooleanV False) _ e env = bigStep env e
ifValue _ _ _ _                  = Nothing
