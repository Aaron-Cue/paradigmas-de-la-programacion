---------- GUIA 0 - REPASO ----------

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use record patterns" #-}


----- EJERCICIO 1
-- usar: null head tail init last take drop (++) concat reverse elem, provenientes del prelude

isNull1 = null [2]
isNull2 = null [2, 3, 5]

lista = [1, 2, 3, 4, 5]
listOfList = [[1], [2, 3, 4]]

cabeza = head lista
cola = tail lista
ultimo = last lista
primeros3 = take 3 lista
deleteFirst2 = drop 2 lista
deleteLast = init lista
concatenacion = [1,2,3] ++ [4,5,6]
concatListOfList = concat listOfList
listaInvertida = reverse lista
pertenece6 = elem 6 lista
pertenece2 = 2 `elem` lista

------ EJERCICIO 2
-- definir las siguientes funciones:
-- a. valorAbsoluto :: Float → Float, que dado un número devuelve su valor absoluto.
-- b. bisiesto :: Int → Bool, que dado un número que representa un año, indica si el mismo es bisiesto.
-- c. factorial :: Int → Int, definida únicamente para enteros positivos, que computa el factorial.
-- d. cantDivisoresPrimos :: Int → Int, que dado un entero positivo devuelve la cantidad de divisores primos.

valorAbsoluto :: Float -> Float
valorAbsoluto x = if x > 0 then x else -x


esDivisible :: Int -> Int -> Bool
esDivisible x y = x `mod` y == 0

bisiesto1 :: Int -> Bool
bisiesto1 year
          | esDivisible year 400 = True
          | esDivisible year 4 && not (esDivisible year 100) = True
          | otherwise = False

bisiesto2 :: Int -> Bool
bisiesto2 year = esDivisible year 400 || esDivisible year 4 && not (esDivisible year 100)

factorial :: Int -> Int
factorial n | n == 1 = 1
            | otherwise = n * factorial (n-1)

esPrimo :: Int -> Bool
esPrimo n | n <= 1 = False
          | otherwise = null ([x | x <- [2..(n-1)], esDivisible n x])

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [x | x <- [1..n], esDivisible n x && esPrimo x]

------ EJERCICIO 3
-- definir las siguientes funciones:
-- a. Definir la función inverso :: Float → Maybe Float que dado un número devuelve su inverso multiplicativo si está definido, o Nothing en caso contrario.
-- b. Definir la función aEntero :: Either Int Bool → Int que convierte a entero una expresión que puede ser booleana o entera. En el caso de los booleanos, el entero que corresponde es 0 para False y 1 para True

inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero = either
      (\n -> n)
      (\bool -> if bool then 1 else 0)

------ EJERCICIO 4
-- definir las siguientes funciones:
-- a. limpiar :: String → String → String, que elimina todas las apariciones de cualquier caracter de la primera cadena en la segunda. Por ejemplo, limpiar ‘‘susto’’ ‘‘puerta’’ evalúa a ‘‘pera’’. Nota: String es un renombre de [Char]. La notación ‘‘hola’’ es equivalente a [‘h’,‘o’,‘l’,‘a’] y a ‘h’:‘o’:‘l’:‘a’:[].
-- b. difPromedio :: [Float] → [Float] que dada una lista de números devuelve la diferencia de cada uno con el promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1].
-- c. todosIguales :: [Int] → Bool que indica si una lista de enteros tiene todos sus elementos iguales.

limpiar1 :: String -> String -> String
limpiar1 [] s2 = s2
limpiar1 (s1:xs1) s2 = limpiar xs1 [letra | letra <- s2, letra /= s1]

limpiar2 :: String -> String -> String
limpiar2 [] s2 = s2
limpiar xs s2 = foldl (\ s2 s1 -> [letra | letra <- s2, letra /= s1]) s2 xs


promedio :: [Float] -> Float
promedio nums = sum nums / fromIntegral (length nums)

difPromedio :: [Float] -> [Float]
difPromedio nums = [n - promedio nums | n <- nums]


todosIguales1 :: [Int] -> Bool
todosIguales1 [] = True
todosIguales1 [x] = True
todosIguales1 (x : xs) = replicate (length xs) x == xs

todosIguales2 :: [Int] -> Bool
todosIguales2 [] = True
todosIguales2 [x] = True
todosIguales2 (x : xs) = all (==x) xs

------ EJERCICIO 5
-- definir las siguientes funciones, teniendo en cuenta el siguiente modelo: data AB a = Nil | Bin (AB a) a (AB a).  
-- a. vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos).
-- b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación de cada uno de los nodos.
-- c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol.

-- tipo de dato AB
data AB a = Nil | Bin (AB a) a (AB a)

-- a.
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

-- arbol vacio
arbolVacio :: AB Int
arbolVacio = Nil

-- arbol con un nodo
arbolConNodo :: AB Int
arbolConNodo = Bin Nil 1 Nil

-- b.
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq a der) = Bin (negacionAB izq) (not a) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq a der) = a * productoAB izq * productoAB der








