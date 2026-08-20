module Laboratorio01 where


distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares ls = sum (map (^2) (filter even ls))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 x y = let promedio = (x + y) / 2
                    resta1 = x - promedio
                    resta2 = y - promedio
                in ((resta1 ^2) + (resta2 ^2)) / 2


clasificaTemperatura :: Int -> String
clasificaTemperatura t
  | t <= 0 = "frio extremo"
  | t <= 15 = "frio"
  | t <= 25 = "templado"
  | t <= 35 = "calido"
  | otherwise = "calor extremo"


intercala :: a -> [a] -> [a]
intercala a [] = []
intercala a [x] = [x]
intercala a (x:xs) = x : a : intercala a xs



data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma x y) = evalua x + evalua y
evalua (Producto x y) = evalua x * evalua y