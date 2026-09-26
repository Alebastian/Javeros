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
curryFun [x] e = Just(Fun x e)
crryFun (x:xs) e 
    | x `elem` xs = Nothing
    | otherwise = case curryFun xs e of
        Just v -> Just(Fun x v)
        Nothing -> Nothing

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA


-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookUp _ [] = Nothing
lookUp _ ((nombre, value):xs)
    | x == nombre = Just value
    | otherwise = lookUpEnv x xs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n) = Just(NumV v)
bigStep _ (Boolean b) = Just(BooleanV b)
bigStep _ Not(e) = do
    v <- bigStep env e
    case v of
        Boolean(b) = Just (BooleanV(not b)))
bigStep env Id(s) = lookUp s xs
bigStep env (Fun x e) = Just(ClousereV x e env )
bigStep env (Add a b) = do
    NumV a <- bigStep env a
    NumV b <- bigStep en v
    Just (NumV (a + b))
bigStep env (Sub a b) = do
    NumV a <- bigStep env a
    NumV b <- bigStep env b
    Just(NumV (max 0 (a b)))
bigStep env (App f a) = do
    ClousureV x c clousereEnv <- bigStep env f


