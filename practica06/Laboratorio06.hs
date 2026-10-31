module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola b e = aux b e 1

aux :: Int -> Int -> Int -> Int
aux _ 0 acc = acc
aux b e acc = aux b (e-1) (b*acc)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS b 0 k = k 1
potenciaCPS b e k = potenciaCPS b (e-1) (\v -> k (b*v))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = aux4 xs 0

aux4 :: [a] -> Int -> Int
aux4 [] acc = acc
aux4 (_:xs) acc = aux4 xs (acc+1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_:xs) k = longitudCPS xs (\v -> k (1+v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = aux1 xs []

aux1 :: [a] -> [a] -> [a]
aux1 [] acc = acc
aux1 (x:xs) acc = aux1 xs (x:acc) 

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k(v++[x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xs = aux2 xs []

aux2 :: [[a]] -> [a] -> [a]
aux2 [] acc = acc 
aux2 (x:xs) acc = aux2 xs (acc ++ x)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (x:xs) k = concatenaCPS xs (\v -> k (x ++ v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = aux3 xs ys []

aux3 :: [a] -> [a] -> [a] -> [a]
aux3 [] ys acc = acc ++ ys
aux3 xs [] acc = acc ++ xs
aux3 (x:xs) (y:ys) acc = aux3 xs ys (acc++ (x:y:[]))

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k (x:y:v))
