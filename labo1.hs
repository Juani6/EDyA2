module Lab01 where

import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
not b = case b of
  True -> False
  False -> True

-- b)
fun :: [a] -> [a]
fun [x]         =  []
fun (x:xs)      =  x : fun xs
fun []          =  error "empty list"

-- c)
len :: [a] -> Int
len []        =  0
len (_:l)     =  1 + len l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail :: Num a => a -> [a] -> [a]
addToTail x xs = map (+x) (tail xs)

-- g)
listmin :: Ord a => [a] -> a
listmin xs = (head . sort) xs

-- h) (*)
smap :: (a -> b) -> [a] -> [b]
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5
-}
five :: a -> Int
five _ = 5

{-
b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado
-}
apply :: (a -> b) -> a -> b
apply f x = f x


{-
c) ident, la función identidad
-}
ident :: a -> a
ident x = x


--d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a,b) -> a
first (x,_) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado
derive :: Fractional a => (a -> a) -> a -> a
derive f x = (f x - f 0.01) / (x - 0.01) 


-- f) sign, la función signo
sign :: (Num a, Ord a) => a -> a
sign x | x < 0 = -1
       | x > 0 = 1
       | x == 0 = error "que haces flaco"

-- g) vabs, la función valor absoluto (usando sign y sin usarla)

vabs1 :: (Num a, Ord a) => a -> a
vabs1 n | n >= 0 = n
        | n < 0  = -n

vabs2 :: (Num a, Ord a) => a -> a
vabs2 x = x * (sign x)


--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero
pot :: Num a => Int -> a -> a
pot 0 y = 1
pot x y = y * (pot (x - 1) y)


--i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor False False = False
xor True True   = False
xor _ _         = True


--j) max3, que toma tres números enteros y devuelve el máximo entre llos

max2 :: Int -> Int -> Int
max2 x y = if x <= y  then y else x
-- lambda pa \x y -> if x <= y then y else x


max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 z (max2 x y)  

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{- 
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
isBisiesto :: Int -> Bool
isBisiesto x | (mod x 400) == 0 = True
             | ((mod x 4) == 0) && ((mod x 100) /= 0)   = True
             | otherwise = False



{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:
-}

(*$) :: Num a => [a] -> a -> [a]
[] *$ x     = []
xs *$ x     = map (*x) xs
--x    *$ []   = []
--x    *$ xs = map (*x) xs
 
v = [1, 2, 3] *$ 2 *$ 4


--5) Definir las siguientes funciones usando listas por comprensión:

--a) 'divisors', que dado un entero positivo 'x' devuelve la
--lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors :: Int -> [Int]
divisors x = if x <= 0 then [] else [n | n <- [1..x], (mod x n) == 0]

--b) 'matches', que dados un entero 'x' y una lista de enteros descarta
--de la lista los elementos distintos a 'x'

matches :: Int -> [Int] -> [Int]
matches x xs = [n | n <- xs, n == x]

--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
--'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
--donde 0 <= a, b, c, d <= 'n'

cuadrupla :: Int ->[(Int,Int,Int,Int)]
cuadrupla x = [(a,b,c,d) | a<-[0..x], 
                           b<-[0..x],
                           c<-[0..x],
                           d<-[0..x],
                           ((a ^ 2) + (b ^ 2)) == ((c ^ 2) + (d ^ 2))]

{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], Lab01.not (elem x (take i xs))]

{- 
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.
Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

--7) Definir mediante recursión explícita
--las siguientes funciones y escribir su tipo más general:

--a) 'suma', que suma todos los elementos de una lista de números
suma :: Num a => [a] -> a
suma []     = 0
suma (x:xs) = x + suma(xs) 


{-
b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario -}

alguno :: [Bool] -> Bool
alguno []     = False
alguno (b:bs) = b || alguno(bs)



-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario
todos :: [Bool] -> Bool
todos []     = True
todos (b:bs) = b && todos bs

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales
codes :: [Char] -> [Int]
codes []     = []
codes (c:cs) = fromEnum c : (codes cs)


-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

restos :: [Int] -> Int -> [Int]
restos []     n = []
restos (x:xs) n = mod x n : (restos xs n)

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados

cuadrados :: Num a => [a] -> [a]
cuadrados []     = []
cuadrados (x:xs) = x*x : cuadrados xs

--g) 'longitudes', que dada una lista de listas, devuelve la
--lista de sus longitudes

longitudes :: [[a]] -> [Int]
longitudes []     = []
longitudes (x:xs) = len(x) : (longitudes xs)

--h) 'orden', que dada una lista de pares de números, devuelve
--la lista de aquellos pares en los que la primera componente es
--menor que el triple de la segunda

orden :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
orden []         = []
orden ((x,y):xs) = if (x < (3 * y)) then (x,y) : (orden xs)
                                    else         (orden xs)

-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares

pares :: [Int] -> [Int]
pares []     = []
pares (x:xs) = if ((mod x 2) == 0) then x : (pares xs)
                                   else     (pares xs)
--j) 'letras', que dada una lista de caracteres, devuelve la
--lista de aquellos que son letras (minúsculas o mayúsculas)

letras :: [Char] -> [Char]
letras []     = []
letras (c:cs) | ((fromEnum c) <= 90) && ((fromEnum c) >= 65)  = c : (letras cs)
              | ((fromEnum c) <= 122) && ((fromEnum c) >= 97) = c : (letras cs)
              | otherwise                                     =     (letras cs)

{-          
k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' 
-}

masDe :: [[a]] -> Int -> [[a]]
masDe []     n = []
masDe (x:xs) n = if ((len x) > n) then x : (masDe xs n)
                                  else     (masDe xs n)
