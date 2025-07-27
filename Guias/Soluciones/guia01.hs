{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map" #-}
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

-- 3.a

sumFoldr :: [Int] -> Int
sumFoldr nums = foldr (+) 0 nums

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr x = foldr (\y acc -> x == y || acc) False

concatFoldr :: [a] -> [a] -> [a]
concatFoldr s1 s2 = foldr (\x acc -> x : acc) s2 s1

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x acc -> if f x then x : acc else acc) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

-- 3.b
-- foldr1 f lista  (acc = last e de la lista)
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x acc -> if f x acc then x else acc)

-- 3.c
sumasParciales :: Num a => [a] -> [a]
sumasParciales lista = foldl (\acc x -> acc ++ [x + sum (take (length acc) lista)]) [] lista


res = sumasParciales [1, 4, -1, 0, 5] -- [1, 5, 4, 4, 9]