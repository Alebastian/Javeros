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
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
    | x `elem` xs = Nothing
    | otherwise = case curryFun xs e of
        Just v  -> Just (Fun x v)
        Nothing -> Nothing

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []     = Nothing
binaryOp _ [_]    = Nothing
binaryOp op (x:xs) = Just (foldl op x xs)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (IdS x)      = Just (Id x)
desugar (NotS e) = do
    de <- desugar e
    Just (Not de)
desugar (AddS args) = mapM desugar args >>= binaryOp Add
desugar (SubS args) = mapM desugar args >>= binaryOp Sub
desugar (FunS params body) = do
    dBody <- desugar body
    curryFun params dBody
desugar (AppS f args) = do
    df   <- desugar f
    dArgs <- mapM desugar args
    curryApp df dArgs
desugar (LetS x e1 e2) = do
    de1 <- desugar e1
    de2 <- desugar e2
    Just (App (Fun x de2) de1)
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e):bindings) body) =
    desugar (LetS x e (LetStarS bindings body))

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((nombre, value):xs)
    | x == nombre = Just value
    | otherwise   = lookupEnv x xs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep _   (Num n)     = Just (NumV n)
bigStep _   (Boolean b) = Just (BooleanV b)
bigStep env (Id s)      = lookupEnv s env
bigStep env (Fun x e)   = Just (ClosureV x e env)
bigStep env (Not e) = do
    v <- bigStep env e
    case v of
        BooleanV b -> Just (BooleanV (not b))
        _          -> Just (BooleanV False)
bigStep env (Add e1 e2) = do
    v1 <- bigStep env e1
    v2 <- bigStep env e2
    case (v1, v2) of
        (NumV n1, NumV n2) -> Just (NumV (n1 + n2))
        _                  -> Nothing
bigStep env (Sub e1 e2) = do
    v1 <- bigStep env e1
    v2 <- bigStep env e2
    case (v1, v2) of
        (NumV n1, NumV n2) -> Just (NumV (max 0 (n1 - n2)))
        _                  -> Nothing
bigStep env (App f a) = do
    cVal <- bigStep env f
    case cVal of
        ClosureV x body closureEnv -> do
            vArg <- bigStep env a
            seq cVal (seq vArg (bigStep ((x, vArg) : closureEnv) body))
        _ -> Nothing

