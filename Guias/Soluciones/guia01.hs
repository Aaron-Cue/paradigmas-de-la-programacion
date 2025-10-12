{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
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

-- >>> permutaciones [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

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
-- entrelazarFoldr [1,2,3] [10,20,30]  ->  [1,34,2,55,3,66]
-- lo que paso:
-- f 1 (f 2 ( (f 3 (const []))) ys
-- primero hace f 3 (const [])   que evalua haciendo la doble beta red a: 

-- (\ys' -> case ys' of
--                 []       -> 3 : const [] []
--                 (y:ys)   -> 3 : y : const [] ys)

-- luego hace:
-- f 2 (\ys' -> case ys' of
--                    []     -> [3]
--                    (y:ys) -> [3, y])

-- osea: 
-- (\x rec ys' -> case ys' of
--                   []     -> x : rec []
--                   (y:ys) -> x : y : rec ys)
-- 2 (\ys' -> case ys' of
--              []     -> [3]
--              (y:ys) -> [3, y])

-- haciendo la beta red queda:
-- (\ys' -> case ys' of
--                 []     -> 2 : rec []
--                 (y:ys) -> 2 : y : rec ys)    rec = (\ys' -> case ys' of
--                                                                  []     -> [3]
--                                                                  (y:ys) -> [3, y])

-- hasta este punto quedaria: evalua ys', entra en el caso (y:ys) => 2 : 10 : rec [20,30]  y asi...

-- EJERCICIO 6

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr' f z xs)
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x : xs) = foldl' f (f z x) xs
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
eliminarPrimeraAparicion e xs = fst (recr f ([], False) xs)
                            where
                              f x xs (rec, elimino)
                                | not elimino && x /= e = (x : rec, elimino)
                                | not elimino && x == e = (rec, True)
                                | otherwise = (x : rec, elimino)

-- 6.b
-- foldr no es util para implementar eliminarPrimeraAparicion porque pide eliminar la primera aparicion y como foldr recorre desde la derecha encontrara las ultimas apariciones (de existir)

-- 6.c
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e [] = [e]
insertarOrdenado e (x : xs) = if e < x then e : x : xs else x : insertarOrdenado e xs


-- EJERCICIO 7

-- 7.a toma una f curry de 2 args, una lista de pares de valores, devuelve la lista de aplicaciones de la funciona  cada par 
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares _ [] = []
mapPares f ((x, y):xs) = f x y : mapPares f xs

-- 7.b
armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x, y) : armarPares xs ys

-- 7.c
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble _ [] [] = []
mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys


-- EJERCICIO 8

-- 8.a
sumaMatriz :: [[Int]] -> [[Int]] -> [[Int]]
sumaMatriz [] [] = []
sumaMatriz (x:xs) (y:ys) = f x y : sumaMatriz xs ys
                          where
                            f [] [] = []
                            f (x':xs') (y':ys') = (x' + y') : f xs' ys'

-- con zipWith 
sumaMatriz' :: [[Int]] -> [[Int]] -> [[Int]]
sumaMatriz' xs ys = zipWith (zipWith (+)) xs ys

-- 8.b
trasponer :: [[Int]] -> [[Int]]
trasponer [] = []
trasponer ([] : _) = []
trasponer xs = map head xs : trasponer (map tail xs)


-- EJERCICIO 9

-- 9.a
foldNat :: a -> (a -> a) -> Int -> a
foldNat z _ 0 = z
foldNat z f n = f (foldNat z f (n - 1))

-- 9.b
potencia :: Int -> Int -> Int
potencia base n = foldNat 1 (* base) n

-- EJERCICIO 10
--10.a
genLista :: a -> (a -> a) -> Int -> [a]
-- genLista _ _ 0 = []
-- genLista e f n = e : genLista (f e) f (n-1)
genLista e f n = foldNat [e] (\xs -> xs ++ [f (last xs)]) (n - 1)

desdeHasta :: Int -> Int -> [Int]
-- desdeHasta d h = [d..h]
desdeHasta d h = genLista d (+1) (h - d)

-- EJERCICIO 11
-- 11.a
data Polinomio a = X
                  | Cte a
                  | Suma (Polinomio a) (Polinomio a)
                  | Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPol x cte suma prod X = x
foldPol x cte suma prod (Cte a) = cte a
foldPol x cte suma prod (Suma p q) =
  suma
    (foldPol x cte suma prod p)
    (foldPol x cte suma prod q)
foldPol x cte suma prod (Prod p q) =
  prod
    (foldPol x cte suma prod p)
    (foldPol x cte suma prod q)

evaluar :: Num a => a -> Polinomio a -> a
evaluar xVal pol = foldPol xVal id (+) (*) pol

-- EJERCICIO 12
data AB a = Nil | Bin (AB a) a (AB a)
-- 12.a
-- estructural
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f Nil = z
foldAB z f (Bin izq val der) = f (foldAB z f izq) val (foldAB z f der)

-- primitiva
recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB z f Nil = z
recAB z f (Bin izq x der) = f izq (recAB z f izq) x der (recAB z f der)

-- 12.b
esNil :: AB a -> Bool
esNil arb = foldAB True (\_ _ _ -> False) arb

altura :: AB a -> Int
altura arb = foldAB 0 (\altIzq valRaiz altDer -> 1 + max altIzq altDer) arb

cantNodos :: AB a -> Int
cantNodos arb = foldAB 0 (\cantIzq valRaiz cantDer -> 1 + cantIzq + cantDer) arb

-- 12.c
mejorSegunAB :: a -> (a -> a -> Bool) -> AB a -> a
mejorSegunAB cb f arb = foldAB cb (\mejIzq valRaiz mejDer -> case (f valRaiz mejIzq, f valRaiz mejDer, f mejIzq mejDer) of
  (True, True, _)    -> valRaiz
  (_, _, True)       -> mejIzq
  _                  -> mejDer) arb

-- EJERCICIO 15
-- 15.a
data RoseTree a = Rose a [RoseTree a]
-- 15.b
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose val hijos) = f val (map (foldRose f) hijos)
-- 15.c
hojas :: RoseTree a -> [a]
hojas = foldRose (\val hijos -> if null hijos then [val] else concat hijos)

distancias :: RoseTree a -> [Int]
distancias = foldRose (\_ hijos -> if null hijos then [0] else map (+1) (concat hijos))

alturaRt :: RoseTree a -> Int
alturaRt = foldRose (\_ hijos -> if null hijos then 1 else 1 + maximum hijos)

type Tono = Integer
data Melodia = Silencio | Nota Tono | Secuencia Melodia Melodia | Paralelo [Melodia]

foldMelo :: b -> (Tono -> b) -> (b -> b -> b) -> ([b] -> b) -> Melodia -> b
foldMelo fSil fNota fSeq fParal melo = case melo of
      Silencio -> fSil
      Nota t -> fNota t
      Secuencia m1 m2 -> fSeq (r m1) (r m2)
        where r = foldMelo fSil fNota fSeq fParal


      Paralelo l -> fParal (map r l)

        where r = foldMelo fSil fNota fSeq fParal

