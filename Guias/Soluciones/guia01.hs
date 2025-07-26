{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import System.Win32 (xBUTTON1)
{-# HLINT ignore "Redundant lambda" #-}



---------- GUIA 1 - PROGRAMACION FUNCIONAL ----------
{- no esta permitido usar recursión explícita, a menos que se indique lo contrario -}


----- EJERCICIO 1

-- 1.a

max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

-- flip :: (a -> b -> c) -> b -> a -> c
subtract1 :: Num a => a -> a -> a
subtract1 = flip (-)

predecesor :: Num a => a -> a
predecesor = subtract1 1

evaluarEnCero :: (Int -> a) -> a
evaluarEnCero = \f -> f 0

-- f :: b -> c
-- g :: a -> b 
-- f.g :: a -> c
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f.f :: 
dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f.f

-- map :: (a -> b) -> [a] -> [b] 
-- flip :: (a -> b -> c) -> b -> a -> c
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

-- 1.b
-- para las funciones anteriores no currificadas, definir su version currificada

max2Curry :: Float -> Float -> Float
max2Curry x y | x > y = x
              | otherwise = y

normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = sqrt (x ^ 2 + y ^ 2)


-- EJERCICIO 2

-- 2.a
-- a curry se le pasa una funcion no curry y dos parametros de forma curry
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

-- 2.b
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

-- 2.c
{- no es posible, ya que en haskell no hay manera (al menos de forma simple) de que la funcion sepa cuantos argumentos tiene de forma arbitraria.
Esto se debe a que en haskell todas las funciones son, de un solo argumento (las que parecen tener varios en realidad devuelven una función que toma el siguiente argumento, y así sucesivamente) -}