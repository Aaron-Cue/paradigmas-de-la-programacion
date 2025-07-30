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

-- 3.d
-- suma alternada de los elems de la lista. osea: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
sumaAlt :: Num a => [a] -> a
                        -- acc = (sumaAlt, Sumar?)
sumaAlt xs = fst (foldr (\x (acc, signoPositivo) -> if signoPositivo then (x + acc, False) else (acc - x, True)) (0, True) xs)

-- usando where
sumaAlt2 :: Num a => [a] -> a
sumaAlt2 xs = fst (foldr f (0, True) xs)
            where
              f x (acc, signoPositivo) | signoPositivo = (x + acc, False)
                                       | otherwise = (acc - x, True)

-- 3.e
sumaAltInversa :: Num a => [a] -> a
sumaAltInversa xs = fst (foldr f (0, False) xs)
                where
                  f x (acc, signoPositivo) | signoPositivo = (x + acc, False)
                                           | otherwise = (acc - x, True)


-- EJERCICIO 4

-- 4.a
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs =
  concatMap
    (\(i, x) -> map (x :) (permutaciones (take i xs ++ drop (i + 1) xs)))
    (zip [0 ..] xs)

-- 4.b
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x : xs) = partes xs ++ map (x :) (partes xs)

-- 4.c 
prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos xs = prefijos (init xs) ++ [xs]


-- EJERCICIO 5
-- 5.a
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x : xs) =
  if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)
-- no usa recursion estructural ya que tanto las variables elementosEnPosicionesPares y xs son usadas en expresiones distintas a elementosEnPosicionesPares xs

-- 5.b
entrelazar :: [a] -> [a] -> [a]
entrelazar [] [] = []
entrelazar (x : xs) ys = if null ys
                          then x : entrelazar xs []
                          else x : head ys : entrelazar xs (tail ys)
-- si usa recursion estructural

entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr [] [] = []
entrelazarFoldr xs ys = foldr (\x rec ys' -> case ys' of
                                    [] -> x : rec []
                                    (y : ys) -> x : y : rec ys)
                                    (const []) xs ys

-- EJERCICIO 6
-- 6.a
eliminarPrimeraAparicion :: Eq a => a -> [a] -> [a]
eliminarPrimeraAparicion _ [] = []
eliminarPrimeraAparicion e (x : xs) = if x == e then eliminarPrimeraAparicion e xs else x : eliminarPrimeraAparicion e xs

-- 6.b
-- foldr no es util para implementar eliminarPrimeraAparicion porque pide eliminar la primera aparicion y como foldr recorre desde la derecha encontrara las ultimas apariciones (de existir)

-- 6.c
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e [] = [e] 
insertarOrdenado e (x : xs) = if e < x then e : x : xs else x : insertarOrdenado e xs