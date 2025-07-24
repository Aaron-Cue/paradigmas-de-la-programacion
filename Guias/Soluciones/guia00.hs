{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Use foldl" #-}
{-# import qualified Main as 2 #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use record patterns" #-}

---------- GUIA 0 - REPASO HASKELL ----------

----- EJERCICIO 1

{-
null toma una lista y devuelve True si la lista está vacía, False en caso contrario.

null :: [a] -> Bool
-}

{-
head toma una lista y devuelve el primer elemento de la misma, si la lista está vacía devuelve error

head :: [a] -> a
-}

{-
tail toma una lista y devuelve la lista sin el primer elemento, si la lista está vacía devuelve error

tail :: [a] -> [a]
-}

{-
init toma una lista y devuelve la lista sin el ultimo elemento, si la lista está vacía devuelve error

init :: [a] -> [a]
-}

{-
last toma una lista y devuelve el ultimo elemento, si la lista está vacía devuelve error

last :: [a] -> a
-}

{-
take toma un numero n y una lista, devuelve una lista con los primeros n elementos

take :: Int -> [a] -> [a]
-}

{-
drop toma un numero n y una lista, devuelve la lista sin los primeros n elementos

drop :: Int -> [a] -> [a]
-}

{-
(++) toma dos listas, concatena las listas en una sola.

(++) :: [a] -> [a] -> [a]
-}

{-
concat toma una lista de listas, devuelve la concatenacion de las sublistas en una lista

concat :: [[a]] -> [a]
-}

{-
reverse toma una lista, devuelve la lista con orden inverso

reverse :: [a] -> [a]
-}

{-
elem toma un elem e y una lista, devuelve True si e pertenece a la lista

elem :: a -> [a] -> Bool
-}

----- EJERCICIO 2

-- 2.a
valorAbsoluto :: Float -> Float
valorAbsoluto x
  | x < 0 = -x
  | otherwise = x

-- 2.b
esDivisible :: Int -> Int -> Bool
esDivisible x y = x `mod` y == 0

bisiesto :: Int -> Bool
bisiesto year = esDivisible year 4 && (not (esDivisible year 100) || esDivisible year 400)

-- 2.c
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 2.d
esPrimo :: Int -> Bool
esPrimo n
  | n < 2 = False
  | otherwise = null [x | x <- [2 .. (n - 1)], esDivisible n x]

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [x | x <- [1 .. n], esDivisible n x && esPrimo x]

-- EJERCICIO 3

{-
data Maybe a = Nothing | Just a -> el tipo Maybe a (con a un tipo cualquiera) devuelve Nothing (indica ausencia de valor) o Just a (indica presencia de un valor de tipo a)

data Either a b = Left a | Right b -> el tipo Either a b (con a y b dos tipos cualquiera) devuelve Left a (en caso de error) o Right b (en caso de exito)
-}

-- 3.a
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

-- 3.b
aEntero1 :: Either Int Bool -> Int
aEntero1 (Left n) = n
aEntero1 (Right bool) = if bool then 1 else 0

aEntero2 :: Either Int Bool -> Int
aEntero2 =
  either
    (\n -> n)
    (\bool -> if bool then 1 else 0)

-- EJERCICIO 4

-- 4.a
-- mediante filter recorro s2 y si no esta en s1, lo dejo, caso contrario elimino
limpiar1 :: String -> String -> String
limpiar1 [] s2 = s2
limpiar1 s1 s2 = filter (\letra -> not (elem letra s1)) s2

-- mediante recursion y lista por comprension, recorro s2 y si no esta en s1 (head de la palabra) la dejo, mismo proceso pero sin el head
limpiar2 :: String -> String -> String
limpiar2 [] s2 = s2
limpiar2 (s1 : xs1) s2 = limpiar2 xs1 [letra | letra <- s2, letra /= s1]

-- recorro s1, aplicando la funcion con el acc
-- la funcion toma el acc y cada elem de s1, devuelve lista donde saco la letra si pertenecia a s1
limpiar3 :: String -> String -> String
limpiar3 [] s2 = s2
limpiar3 s1 s2 = foldl (\s2 xs -> [letra | letra <- s2, letra /= xs]) s2 s1

-- 4.b
promedio :: [Float] -> Float
promedio xs = sum xs / fromIntegral (length xs)

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio nums = map (\num -> num - promedio nums) nums

-- 4.c
todosIguales1 :: [Int] -> Bool
todosIguales1 (x : xs) = all (== x) (x : xs)

todosIguales2 :: [Int] -> Bool
todosIguales2 (x : xs) = replicate (length xs) x == xs

-- EJERCICIO 5
data AB a = Nil | Bin (AB a) a (AB a)

-- el tipo AB a (con a un tipo cualquiera) devuelve Nil en caso de arbol vacio o los subarboles y el valor del nodo

-- 5.a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- 5.b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq v der) = Bin (negacionAB izq) (not v) (negacionAB der)

-- 5.c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq v der) = v * productoAB izq * productoAB der